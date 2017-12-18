import java.io.{File, PrintWriter}
import java.net.URL

import exercice1.Crawler
import exercice2._
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.SparkContext._
import org.apache.spark.graphx.{Edge, Graph, VertexId, VertexRDD}
import org.apache.spark.rdd.RDD
import org.apache.spark.rdd.RDD._
import spire.std.map

import scala.collection.mutable.ArrayBuffer

object Main extends App {


  val conf = new SparkConf()
      .setAppName("BDR TP2")
      .setMaster("local[1]")
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

        println("Fin Exercice 1");
    }


    //def mergeMessage
    def exercice2Partie1() : Unit = {
        val monstres: RDD[(VertexId,Monstre)] =
            sc.parallelize(Array(
                (1L,new AngelSolar(new Position(1,5),1,15,363,true,true)),
                (2L,new WorgsRider(new Position(111,9),2,10,13)),
                (3L,new WorgsRider(new Position(111,8),2,9,13)),
                (4L,new WorgsRider(new Position(111,7),2,5,13)),
                (5L,new WorgsRider(new Position(111,6),2,3,13)),
                (6L,new WorgsRider(new Position(111,5),2,12,13)),
                (7L,new WorgsRider(new Position(111,4),2,11,13)),
                (8L,new WorgsRider(new Position(111,3),2,5,13)),
                (9L,new WorgsRider(new Position(111,2),2,6,13)),
                (10L,new WorgsRider(new Position(111,1),2,1,13)),
                (11L,new BarbareOrc(new Position(120,7),2,8,142)),
                (12L,new BarbareOrc(new Position(120,6),2,7,142)),
                (13L,new BarbareOrc(new Position(120,4),2,12,142)),
                (14L,new BarbareOrc(new Position(120,3),2,13,142)),
                (15L,new LeWarlord(new Position(125,5),2,15,141))
            ))

        val vertex: RDD[Edge[EdgeProperty]] =
            sc.parallelize(Array(
                //Solar
                Edge(1L, 2L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(1L, 3L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(1L, 4L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(1L, 5L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(1L, 6L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(1L, 7L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(1L, 8L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(1L, 9L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(1L, 10L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(1L, 11L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(1L, 12L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(1L, 13L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(1L, 14L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(1L, 15L, new EdgeProperty(TypeRelation.ENEMY)),

                //WorgRider1
                Edge(2L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(2L, 3L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(2L, 4L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(2L, 5L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(2L, 6L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(2L, 7L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(2L, 8L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(2L, 9L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(2L, 10L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(2L, 11L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(2L, 12L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(2L, 13L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(2L, 14L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(2L, 15L, new EdgeProperty(TypeRelation.FRIEND)),
                //WorgRider2
                Edge(3L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(3L, 2L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(3L, 4L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(3L, 5L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(3L, 6L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(3L, 7L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(3L, 8L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(3L, 9L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(3L, 10L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(3L, 11L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(3L, 12L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(3L, 13L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(3L, 14L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(3L, 15L, new EdgeProperty(TypeRelation.FRIEND)),
                //WorgRider3
                Edge(4L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(4L, 2L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(4L, 3L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(4L, 5L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(4L, 6L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(4L, 7L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(4L, 8L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(4L, 9L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(4L, 10L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(4L, 11L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(4L, 12L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(4L, 13L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(4L, 14L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(4L, 15L, new EdgeProperty(TypeRelation.FRIEND)),
                //WorgRider4
                Edge(5L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(5L, 2L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(5L, 4L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(5L, 3L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(5L, 6L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(5L, 7L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(5L, 8L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(5L, 9L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(5L, 10L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(5L, 11L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(5L, 12L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(5L, 13L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(5L, 14L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(5L, 15L, new EdgeProperty(TypeRelation.FRIEND)),
                //WorgRider5
                Edge(6L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(6L, 2L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(6L, 4L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(6L, 5L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(6L, 3L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(6L, 7L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(6L, 8L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(6L, 9L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(6L, 10L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(6L, 11L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(6L, 12L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(6L, 13L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(6L, 14L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(6L, 15L, new EdgeProperty(TypeRelation.FRIEND)),
                //WorgRider6
                Edge(7L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(7L, 2L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(7L, 4L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(7L, 5L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(7L, 6L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(7L, 3L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(7L, 8L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(7L, 9L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(7L, 10L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(7L, 11L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(7L, 12L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(7L, 13L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(7L, 14L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(7L, 15L, new EdgeProperty(TypeRelation.FRIEND)),
                //WorgRider7
                Edge(8L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(8L, 2L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(8L, 4L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(8L, 5L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(8L, 6L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(8L, 7L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(8L, 3L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(8L, 9L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(8L, 10L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(8L, 11L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(8L, 12L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(8L, 13L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(8L, 14L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(8L, 15L, new EdgeProperty(TypeRelation.FRIEND)),
                //WorgRider8
                Edge(9L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(9L, 2L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(9L, 4L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(9L, 5L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(9L, 6L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(9L, 7L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(9L, 8L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(9L, 3L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(9L, 10L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(9L, 11L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(9L, 12L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(9L, 13L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(9L, 14L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(9L, 15L, new EdgeProperty(TypeRelation.FRIEND)),
                //WorgRider9
                Edge(10L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(10L, 2L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(10L, 4L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(10L, 5L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(10L, 6L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(10L, 7L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(10L, 8L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(10L, 9L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(10L, 3L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(10L, 11L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(10L, 12L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(10L, 13L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(10L, 14L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(10L, 15L, new EdgeProperty(TypeRelation.FRIEND)),
                //BarbarOrc1
                Edge(11L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(11L, 2L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(11L, 4L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(11L, 5L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(11L, 6L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(11L, 7L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(11L, 8L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(11L, 3L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(11L, 10L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(11L, 9L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(11L, 12L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(11L, 13L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(11L, 14L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(11L, 15L, new EdgeProperty(TypeRelation.FRIEND)),
                //BarbarOrc2
                Edge(12L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(12L, 2L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(12L, 4L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(12L, 5L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(12L, 6L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(12L, 7L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(12L, 8L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(12L, 3L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(12L, 10L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(12L, 11L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(12L, 9L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(12L, 13L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(12L, 14L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(12L, 15L, new EdgeProperty(TypeRelation.FRIEND)),
                //BarbarOrc3
                Edge(13L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(13L, 2L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(13L, 4L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(13L, 5L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(13L, 6L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(13L, 7L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(13L, 8L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(13L, 3L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(13L, 10L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(13L, 11L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(13L, 12L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(13L, 9L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(13L, 14L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(13L, 15L, new EdgeProperty(TypeRelation.FRIEND)),
                //BarbarOrc4
                Edge(14L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(14L, 2L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(14L, 4L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(14L, 5L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(14L, 6L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(14L, 7L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(14L, 8L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(14L, 3L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(14L, 10L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(14L, 11L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(14L, 12L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(14L, 13L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(14L, 9L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(14L, 15L, new EdgeProperty(TypeRelation.FRIEND)),
                //Warlord 1
                Edge(15L, 1L, new EdgeProperty(TypeRelation.ENEMY)),
                Edge(15L, 2L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(15L, 4L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(15L, 5L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(15L, 6L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(15L, 7L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(15L, 8L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(15L, 3L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(15L, 10L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(15L, 11L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(15L, 12L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(15L, 13L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(15L, 14L, new EdgeProperty(TypeRelation.FRIEND)),
                Edge(15L, 9L, new EdgeProperty(TypeRelation.FRIEND))
            ))

        var graph = Graph(monstres,vertex)


        def executionDeLalgorithme() : Unit = {
          while (true){
            println("------------------------------------------------------------------------------")
            println("                        ETAT  INITIAL                                         ")
            println("------------------------------------------------------------------------------")

            graph.vertices.collect().foreach(println)



            var actionTodo: VertexRDD[ArrayBuffer[msg]] = graph.aggregateMessages[ArrayBuffer[msg]](
              triplet =>{
                triplet.srcAttr.actionPossible(triplet)
              },
              (msg1,msg2) => msg1 ++ msg2
            )

            //Condition d'arret
            if(actionTodo.collect().length == 0) return


            println("------------------------------------------------------------------------------")
            println("                        ACTIONS POSSIBLES                                     ")
            println("------------------------------------------------------------------------------")

            //var ttt = actionTodo.collect().foreach(println)


            //Choix des actions
            graph = graph.joinVertices(actionTodo)(
              (vid, monstres, msgs) => monstres.choisirAction(vid,monstres,msgs)
            )

            //var res = graph.vertices.take(graph.numVertices.toInt)

            println("------------------------------------------------------------------------------")
            println("                       CHOIX ACTION                                      ")
            println("------------------------------------------------------------------------------")

            graph.vertices.collect().foreach(println)

            val executerLesAction:VertexRDD[ArrayBuffer[message2]] = graph.aggregateMessages[ArrayBuffer[message2]](
              triplet =>{
                triplet.srcAttr.executeAction(triplet)
              },
              (msg1,msg2) => msg1 ++ msg2
            )


            println("------------------------------------------------------------------------------")
            println("                        EXECUTE ACTIONS                                     ")
            println("------------------------------------------------------------------------------")
            executerLesAction.collect().foreach(println)

            graph = graph.joinVertices(executerLesAction)(
              (vid,monstres,msgs) => monstres.receptionnerAction(vid,monstres,msgs)
            )

            println("------------------------------------------------------------------------------")
            println("                        RECEPTION ACTION                                     ")
            println("------------------------------------------------------------------------------")
            graph.vertices.collect().foreach(println)

            //val res2 = graph.vertices.take(graph.numVertices.toInt)
            graph = graph.subgraph(vpred = (id, attr) => attr.getPDV()> 0)
          }
        }

      executionDeLalgorithme()
      println("Fin Combat 1")

    }

    exercice2Partie1()
    //exercice1();



}