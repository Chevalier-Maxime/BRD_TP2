package exercice1_cluster

import java.io.{File, PrintWriter}
import java.net.URL

import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}


object UnSeulFichier extends App {

  val conf = new SparkConf()
    .setAppName("BDR TP2")
    .setMaster("local[1]")
  val sc = new SparkContext(conf)
  sc.setLogLevel("ERROR")


  def exercice1(): Unit = {
    val c = new Crawler(new URL("http://paizo.com/pathfinderRPG/prd/bestiary/monsterIndex.html"))
    c.crawlerMainPage()

    val distData = sc.parallelize(c.collection)

    var RDDflatMap: RDD[(String, String)] = distData.flatMap(creature => {
      var retour = Seq[(String, String)]()

      for (spell <- creature.spells) {
        //println("Ok : " + (spell,creature.name));
        retour = retour :+ (spell, creature.name)
      }
      retour
    })

    // Verifier l'affichage du RDD
    //println(RDDflatMap.collect().foreach(println));


    var RDDreduce: RDD[(String, String)] = RDDflatMap.reduceByKey((a, b) => {
      a + " / " + b
    })

    //println(RDDreduce.collect().foreach(println));
    val writer = new PrintWriter(new File("result.txt"))


    for (entry <- RDDreduce.collect()) {

      entry match {
        case (a, b) => writer.write("Sort : " + a + "\t is used by : " + b + "\r\n")
      }
    }

    writer.close()

    println("Fin Exercice 1");

  }

  exercice1()

}

class Creature(val name: String) extends Serializable {
  var spells = scala.collection.mutable.Set[String]()

  def addspell(spell: String): Unit = {
    spells += spell
  }

  def toJson():JObject = {
    val json =
      ( //"creature" ->
        ("name" -> name) ~
          ("spells" -> spells)
        )


    return json //compact(render(json))
  }
}

class Crawler (URL: URL){

  var file: String = "creatures.json"
  val writer = new PrintWriter(new File(file))
  var collection = new ListBuffer[Creature]()

  def writeFile() = {
    val json = (
      ("creatures" -> collection.map {
        c => c.toJson()
      })
      )
    writer.write(compact(render(json)))
  }

  def processBlock(elementsBetween: ListBuffer[Element], nomCreature: String) = {
    var blop = gethref(elementsBetween)
    var c = new Creature(nomCreature)
    for(element <- blop){
      if(element.attr("href").contains("/spells/")){
        val pattern = new Regex("([#])\\w+")
        val url = (element.attr("href").split("/").last)
        val nom = url.split("\\."){0}
        c.addspell(nom)
      }
    }
    collection.add(c)
  }

  def gethref(elementsBetween: ListBuffer[Element]) :ListBuffer[Element] = {
    var listHref = new ListBuffer[Element]()
    for(element <- elementsBetween){
      listHref.addAll(gethrefRec(element))
    }
    return listHref
  }

  def gethrefRec(element: Element) :ListBuffer[Element] = {
    var elementsBetween = new ListBuffer[Element]()
    for(child <- element.children()){
      elementsBetween.addAll(gethrefRec(child))
    }
    if(element.attributes().hasKey("href")){
      elementsBetween.add(element)
    }
    return elementsBetween
  }





  def crawlerCreaturePage(e: Element) = {
    //utiliser ca : https://stackoverflow.com/questions/6534456/jsoup-how-to-get-all-html-between-2-header-tags
    val url = e.child(0).attr("abs:href")
    val doc = Jsoup.connect(url).get()

    val firstH1 = doc.select("h1").first()
    val siblings = firstH1.siblingElements()
    var elementsBetween = new ListBuffer[Element]()// = ListBuffer()
    elementsBetween.add(firstH1)
    var nomCreature: String = firstH1.text()

    var trouve : Boolean = false;
    for ( i <-0 to siblings.size()-1) {
      val sibling = siblings.get(i)
      if (!((sibling.tagName().equals("h1")) ) ) {
        elementsBetween.+=:(sibling)
        if((sibling.attr("class").contains("stat-block-title"))) {
          trouve = true
          /*try {
            nomCreature = nomCreature + sibling.child(0).childNode(0).toString
          }catch { //Animaux compagnons
            case _: Throwable => println(url)
              elementsBetween = ListBuffer()
              trouve = false
          }*/
        }
      } else {
        if(trouve){
          processBlock(elementsBetween, nomCreature)
          elementsBetween = ListBuffer()
        }
        else{
          elementsBetween = ListBuffer()
        }
        nomCreature = sibling.text()
        trouve = false
      }
    }
    if(!elementsBetween.isEmpty && trouve){
      processBlock(elementsBetween,nomCreature)
    }
  }

  def crawlerOnThisLetter(elements: Elements) = {
    for (e <- elements){
      if(e.childNodeSize()==1){
        crawlerCreaturePage(e)
      }
    }
  }

  def crawlerMainPage() = {
    val doc = Jsoup.connect(URL.toString).get()
    val elements = doc.select("div#monster-index-wrapper").first()
    //traiter le premier
    for(e <- elements.children()){
      if(e.tagName().equals("ul")){
        crawlerOnThisLetter(e.children())
      }
    }

    writeFile()

  }



}