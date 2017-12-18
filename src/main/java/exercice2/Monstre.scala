package exercice2

import exercice2.TypeAction.TypeAction
import org.apache.spark.graphx.{EdgeContext, VertexId}

import scala.collection.mutable.ArrayBuffer

abstract class Monstre(
                      var position:Position,
                      nom:String,
                      equipe:Int,
                      PDVmax:Int,
                      Lvl:Int,
                      deplacementParTour:Int,
                      armure:Int,
                      var PDV:Int
                      ) extends Serializable {

  var vivant: Boolean = true;

  def removePDV(degats: Int): Unit = {
    if(PDV > 0) {
      this.PDV -= degats
      if (PDV < 0) {
        vivant = false
        println(this + " meurt")
      }
    }
  }

  def getArmure(): Int ={
    this.armure
  }
  def setPosition(position: Position): Unit = {
    this.position = position
  }


  def receptionnerAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[message2]): _root_.exercice2.Monstre

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
   this.PDV
  }
  var actionsPossibles = scala.collection.mutable.Seq[ActionMonstre]();

  def distance(position: Position): Int ={
    val x:Double = (this.position.x - position.x).toDouble
    val carreX:Double = Math.pow(x,2.0)

    val y:Double = (this.position.y - position.y).toDouble
    val carreY:Double = Math.pow(y,2.0)

    val res = Math.sqrt(carreX + carreY).toInt
    if (res == 0 )
      1
    else
      res
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
    super.toString + " next : " + this.nextAction + " en vie :" + this.vivant + " position : " + this.position
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
  def d8() : Int ={
    scala.util.Random.nextInt(8)+1
  }
}
