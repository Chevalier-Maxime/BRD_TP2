package main.java

import java.net.URL

import creature.Creature
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

import scala.collection.JavaConversions._

class Crawler (URL: URL){

  def crawlerCreaturePage(e: Element) = {
    val url = e.child(0).attr("abs:href")
    val doc = Jsoup.connect(url).get()
    val elements = doc.select("div.body").first()
    val nom = elements.select("p.stat-block-title").first().child(0).childNode(0).toString
    val creature = new Creature(nom)

    for(child <- elements.select("a[href]")){
      if(child.attr("href").contains("/spells/"))
        System.out.println("Blop") //TODO On a le lien du spell, trouver son nom et ajouter a la créature.
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
      System.out.println("Blop")
    }
    System.out.println("Blop")


  }



}
