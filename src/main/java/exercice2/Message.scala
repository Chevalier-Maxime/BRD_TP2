package exercice2

import exercice2.TypeAction.TypeAction
import org.apache.spark.graphx.VertexId

object TypeAction extends Enumeration with Serializable {
  type TypeAction = Value
  val HEAL, ATTAQUE, MOVE = Value
}

case class msg(var actionType : TypeAction, var idDest : VertexId, var posSrc:Position) extends Serializable {

  def getType : TypeAction ={
    actionType
  }
}

abstract class message2 (actionType : TypeAction) extends Serializable {
  def getActionType : TypeAction ={
     actionType
  }
}

case class heal(var Lvl : Int, var multiplicateur : Int) extends message2 (TypeAction.HEAL){

  def getLvl : Int ={
     Lvl
  }
  def getMultiplicateur : Int ={
    multiplicateur
  }

}
case class attaque(var toucher : Int, var degats : Int) extends message2 (TypeAction.ATTAQUE){

  def getToucher() : Int ={
    return toucher
  }
  def  detDegats() : Int ={
    return degats
  }

}

case class deplacement(position: Position) extends message2 (TypeAction.MOVE){
  def getPosition: Position ={
    position
  }
}
