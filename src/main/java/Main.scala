import java.net.URL

import main.java.Crawler
import org.apache.spark.SparkConf

object Main extends App {

    val c = new Crawler(new URL("http://paizo.com/pathfinderRPG/prd/bestiary/monsterIndex.html"))
    c.crawlerMainPage()

    val conf = new SparkConf()
}
