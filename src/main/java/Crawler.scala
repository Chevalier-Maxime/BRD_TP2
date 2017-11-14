package main.java

import java.net.URL

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.select.Elements

import scala.collection.JavaConversions._


class Crawler (URL: URL){

  def crawlerCreaturePage(e: Element) = {
    val url = e.attr("abs:href")
    val doc = Jsoup.connect(url).get()
    val elements = doc.select("div#body").first()
    val nom = elements.select("p#stat-block-title").first()
    //val creature = new Creature()

    for(child <- elements.children()){

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
