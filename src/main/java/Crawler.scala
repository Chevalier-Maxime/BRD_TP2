package main.java

import java.net.URL

import creature.Creature
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

class Crawler (URL: URL){

  def processBlock(elementsBetween: ListBuffer[Elements]) = {

  }

  def crawlerCreaturePage(e: Element) = {
    //TODO différencier les pages avec un seul monstre et celles avec plusieurs. (via get parent ?) ou via stat-block-title (dans le HTML)
    //utiliser ca : https://stackoverflow.com/questions/6534456/jsoup-how-to-get-all-html-between-2-header-tags
    val url = e.child(0).attr("abs:href")
    val doc = Jsoup.connect(url).get()

    val firstBlock = doc.select("p.stat-block-title").first()
    val siblings = firstBlock.siblingElements()
    var elementsBetween : ListBuffer[Elements] = ListBuffer()

    var trouve : Boolean = false;
    for ( i <-1 to siblings.size()) {
      var sibling = siblings.get(i)
      if (!((sibling.tag().equals("p")) && (sibling.attr("class").contains("stat-block-title"))) && trouve) {
        sibling +: elementsBetween
      } else {
        if (trouve) {
          processBlock(elementsBetween)
          elementsBetween = ListBuffer()
        } else {
          if ((sibling.attr("class").contains("stat-block-title")) && !trouve) {
            elementsBetween = ListBuffer()
          }
        }
      }
    }
    if(!elementsBetween.isEmpty){
      processBlock(elementsBetween)
    }



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
