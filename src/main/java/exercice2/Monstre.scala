package exercice2

import exercice2.TypeAction.TypeAction
import org.apache.spark.graphx.{EdgeContext, VertexId}

import scala.collection.mutable.ArrayBuffer

abstract class Monstre(
                      position:Position,
                      nom:String,
                      equipe:Int,
                      PDVmax:Int
                      ) extends Serializable {

  def receptionnerAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[msg]): _root_.exercice2.Monstre = {
    var messagePrint = "Moi "+nom+"@"+vid+" recoit les differentes actions :"
    msgs.foreach(message => message.getType() match {
      case TypeAction.MOVE => messagePrint += "MOVE ";
      case TypeAction.ATTAQUE => messagePrint +="ATTAQUE ";
      case TypeAction.HEAL => messagePrint += "SOIN"
    })

    println(messagePrint)

    //this.nextAction = null
    this
  }
def getNom() : String = {
  this.nom
}

  //TODO Changer le type de message
  def executeAction(triplet: EdgeContext[Monstre, EdgeProperty, ArrayBuffer[msg]]): Unit = {
    println(triplet.srcId + triplet.srcAttr.getNom())
    //var (id, actionType) = this.nextAction
    if(triplet.dstId == this.nextAction._1){
      nextAction._2 match {
        case TypeAction.HEAL => println( triplet.srcId + " heal " + triplet.dstId)
          val m = new ArrayBuffer[msg]()
          m.append(new msg(TypeAction.HEAL,triplet.srcId,triplet.dstAttr.getPosition()));
        case TypeAction.MOVE => println(triplet.srcId + " se deplace vers " + triplet.dstId);
          //TODO Move
          val m = new ArrayBuffer[msg]()
          m.append(new msg(TypeAction.MOVE,triplet.srcId,triplet.dstAttr.getPosition()));
        case TypeAction.ATTAQUE => println(triplet.srcId + " attaque " + triplet.dstId)
          val m = new ArrayBuffer[msg]()
          m.append(new msg(TypeAction.ATTAQUE,triplet.srcId,triplet.dstAttr.getPosition()));
      }
    }
  }

  def choisirAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[msg]): _root_.exercice2.Monstre


  def actionPossible(triplet: EdgeContext[Monstre, EdgeProperty, ArrayBuffer[msg]]): Unit


  var nextAction: (VertexId,TypeAction) = null

  var PDV : Int = PDVmax

  def getPDVMax() : Int = {
    return PDVmax
  }

  def getPosition() : Position = {
    return position
  }


  def getPDV() : Int = {
    return PDV
  }
  var actionsPossibles = scala.collection.mutable.Seq[ActionMonstre]();

  def distance(position: Position): Int ={
    val x:Double = (this.position.x - position.x).toDouble
    val carreX:Double = Math.pow(x,2.0)

    val y:Double = (this.position.y - position.y).toDouble
    val carreY:Double = Math.pow(y,2.0)

    Math.sqrt(carreX + carreY).toInt
  }


}
