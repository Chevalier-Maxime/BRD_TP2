package main.java

import java.net.URL

import creature.Creature
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

class Crawler (URL: URL){

  def processBlock(elementsBetween: ListBuffer[Element], nomCreature: String) :Creature = {
    var blop = gethref(elementsBetween)
    var c = new Creature(nomCreature)
    for(element <- blop){
      if(element.attr("href").contains("/spells/")){
        System.out.println("Blop") //TODO On a le lien du spell, trouver son nom et ajouter a la créature.
      }
    }
    return c
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
    //TODO faire entre H1 car c'est un parent en fait ...
    //utiliser ca : https://stackoverflow.com/questions/6534456/jsoup-how-to-get-all-html-between-2-header-tags
    val url = e.child(0).attr("abs:href")
    val doc = Jsoup.connect(url).get()

    val firstH1 = doc.select("h1").first()
    val siblings = firstH1.siblingElements()
    var elementsBetween = new ListBuffer[Element]()// = ListBuffer()
    elementsBetween.add(firstH1)
    var nomCreature: String ="" // = firstH1.child(0).childNode(0).toString

    var trouve : Boolean = false;
    for ( i <-0 to siblings.size()-1) {
      val sibling = siblings.get(i)
      if (!((sibling.tagName().equals("h1")) ) ) {
        elementsBetween.+=:(sibling)
        if((sibling.attr("class").contains("stat-block-title"))) {
          trouve = true
          nomCreature = sibling.child(0).childNode(0).toString
        }
      } else {
         if(trouve){
            processBlock(elementsBetween, nomCreature)
            elementsBetween = ListBuffer()
          }
        else{
           elementsBetween = ListBuffer()
         }
        trouve = false
      }
    }
    if(!elementsBetween.isEmpty && trouve){
      processBlock(elementsBetween,nomCreature)
    }

    System.out.println("Blop")

//    val elements = doc.select("div.body").first()
//    val nom = elements.select("p.stat-block-title").first().child(0).childNode(0).toString
//    val creature = new Creature(nom)
//
//    for(child <- elements.select("a[href]")){
//      if(child.attr("href").contains("/spells/")){
//        System.out.println("Blop") //TODO On a le lien du spell, trouver son nom et ajouter a la créature.
//      }
//    }



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
      System.out.println("Blop")
    }
    System.out.println("Blop")


  }



}
