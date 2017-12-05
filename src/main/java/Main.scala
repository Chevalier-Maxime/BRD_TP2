import java.io.{File, PrintWriter}
import java.net.URL

import main.java.Crawler
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import org.apache.spark.rdd.RDD._
import spire.std.map

object Main extends App {

    val conf = new SparkConf()
      .setAppName("BDR TP2")
      .setMaster("local[*]")
    val sc = new SparkContext(conf)
    sc.setLogLevel("ERROR")


    def exercice1() : Unit ={
        val c = new Crawler(new URL("http://paizo.com/pathfinderRPG/prd/bestiary/monsterIndex.html"))
        c.crawlerMainPage()

        val distData = sc.parallelize(c.collection)

        /* bb = bb.flatMap( he => {
             var ret = Seq[hyperedge]()
             val ttt = he.set -- bv.value.set
             val id = he.edgeID
             if (ttt.isEmpty == false) {
                 ret = ret :+ hyperedge(ttt,id)
             }
             ret
         })*/


        var RDDflatMap: RDD[(String, String)] = distData.flatMap(creature => {
            var retour = Seq[(String,String)]()

            for(spell <- creature.spells ) {
                //println("Ok : " + (spell,creature.name));
                retour = retour :+ (spell,creature.name)
            }
            retour
        })

        // Verifier l'affichage du RDD
        //println(RDDflatMap.collect().foreach(println));


        var RDDreduce: RDD[(String, String)] = RDDflatMap.reduceByKey((a, b) => {
            a + " / " +  b
        })

        //println(RDDreduce.collect().foreach(println));
        val writer = new PrintWriter(new File("result.txt" ))


        for( entry <- RDDreduce.collect()) {

            entry match {
                case (a,b) => writer.write("Sort : " + a + "\t is used by : " + b + "\r\n")
            }
        }

        writer.close()

    }



    exercice1();

}