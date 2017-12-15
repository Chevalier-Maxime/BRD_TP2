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
    println(this)
    //var (id, actionType) = this.nextAction
    if(triplet.dstId == nextAction.vertexId){
      nextAction.typeAction match {
        case TypeAction.HEAL => println( triplet.srcId + " heal " + triplet.dstId)
          val m = new ArrayBuffer[msg]()
          m.append(new msg(TypeAction.HEAL,triplet.srcId,triplet.dstAttr.getPosition()));
          triplet.sendToSrc(m)
        case TypeAction.MOVE => println(triplet.srcId + " se deplace vers " + triplet.dstId);
          //TODO Move
          val m = new ArrayBuffer[msg]()
          m.append(new msg(TypeAction.MOVE,triplet.srcId,triplet.dstAttr.getPosition()));
          triplet.sendToSrc(m)
        case TypeAction.ATTAQUE => println(triplet.srcId + " attaque " + triplet.dstId)
          val m = new ArrayBuffer[msg]()
          m.append(new msg(TypeAction.ATTAQUE,triplet.srcId,triplet.dstAttr.getPosition()));
          triplet.sendToSrc(m)
      }
    }
  }

  def choisirAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[msg]): _root_.exercice2.Monstre


  def actionPossible(triplet: EdgeContext[Monstre, EdgeProperty, ArrayBuffer[msg]]): Unit


  case class prochaineAction(var vertexId: VertexId, var typeAction: TypeAction)


  //TODO Cette variable se réinitialise !
  var nextAction :prochaineAction = null// prochaineAction(-1,TypeAction.HEAL) //On initialise car sinon c'est la merde

  var PDV : Int = PDVmax

  def getPDVMax() : Int = {
    return PDVmax
  }

  def getPosition() : Position = {
    return position
  }

  def setNextAction(vertexId: VertexId,typeAction: TypeAction): Unit ={
    this.nextAction = prochaineAction(vertexId,typeAction)
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


  override def toString: String = {
    super.toString + " next : " + this.nextAction
  }
}
