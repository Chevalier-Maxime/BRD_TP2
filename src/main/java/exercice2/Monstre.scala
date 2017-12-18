package exercice2

import exercice2.TypeAction.TypeAction
import org.apache.spark.graphx.{EdgeContext, VertexId}

import scala.collection.mutable.ArrayBuffer

abstract case class Monstre(
                      var position:Position,
                      nom:String,
                      equipe:Int,
                      PDVmax:Int,
                      Lvl:Int,
                      deplacementParTour:Int,
                      armure:Int
                      ) extends Serializable {
  def getArmure(): Int ={
    this.armure
  }


  def setPosition(position: Position): Unit = {
    this.position = position
  }


  def receptionnerAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[message2]): _root_.exercice2.Monstre = {
    var messagePrint = "Moi "+monstres.getNom()+"@"+vid+" recoit les differentes actions :"
    msgs.foreach(message => message.getActionType match {
      case TypeAction.MOVE => messagePrint += "MOVE ";
        val messageDepl = message.asInstanceOf[deplacement]
        monstres.setPosition(messageDepl.getPosition)
      case TypeAction.ATTAQUE => messagePrint +="ATTAQUE ";
      case TypeAction.HEAL => messagePrint += "SOIN"
        val messageHeal = message.asInstanceOf[heal]
        monstres.addPDV(messageHeal.Lvl*messageHeal.multiplicateur)
    })

    println(messagePrint)

    //this.nextAction = null
    monstres
  }

  def getNom() : String = {
    this.nom
  }
  def getLvl() : Int = {
    this.Lvl
  }
  def getEquipe() :Int = {
    this.equipe
  }
  def getDeplacementParTour : Int = {
    deplacementParTour
  }

  //TODO Changer le type de message
  def executeAction(triplet: EdgeContext[Monstre, EdgeProperty, ArrayBuffer[message2]]): Unit

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




def calculDeplacement( positionAAtteindre: Position, nbDeplacement : Int): Position ={
  var dpl = nbDeplacement
  //On se déplace sur les x
  var directionX = 0
  if(positionAAtteindre.x < position.x)
    directionX = -1
  else
    directionX = 1

  var distX = Math.abs(position.x - positionAAtteindre.x)
  var newX = 0
  if (distX < dpl){
    dpl -= distX
    newX = position.x + (distX * directionX)

    var directionY = 0
    if(positionAAtteindre.y < position.y)
      directionY = -1
    else
      directionY = 1

    var distY = Math.abs(position.y - positionAAtteindre.y)
    var newY = 0
    if (distY < dpl) {
      newY = position.y + (distY * directionY)
      return new Position(newX,newY)
    }
    else{
      newY = position.y + (dpl * directionY)
      return new Position(newX,newY)
    }
  }else{
    newX = position.x + (dpl * directionX)
    new Position(newX, position.y)
  }

  }

  override def toString: String = {
    super.toString + " next : " + this.nextAction
  }

  def addPDV(vie: Int): Unit ={
    var total = this.getPDV() + vie
    if(total > this.getPDVMax())
      this.PDV = this.getPDVMax()
    else
      this.PDV = total
  }
  def d20() : Int ={
    scala.util.Random.nextInt(20)+1
  }
  def d6() : Int ={
    scala.util.Random.nextInt(6)+1
  }
}
