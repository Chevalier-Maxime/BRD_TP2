package exercice2

import exercice2.TypeAction.TypeAction
import org.apache.spark.graphx.VertexId

object TypeAction extends Enumeration with Serializable {
  type TypeAction = Value
  val HEAL, ATTAQUE, MOVE = Value
}

case class msg(var actionType : TypeAction, var idDest : VertexId, var posSrc:Position) extends Serializable {

  def getType() : TypeAction ={
    return actionType
  }
}

abstract class message2 (actionType : TypeAction){
  def getActionType() : TypeAction ={
    return actionType
  }
}

case class heal(var Lvl : Int, var multiplicateur : Int) extends message2 (TypeAction.HEAL){

  def getLvl() : Int ={
    return Lvl
  }
  def getMultiplicateur : Int ={
    return  multiplicateur
  }

}
case class attaque() extends message2 (TypeAction.ATTAQUE){

}