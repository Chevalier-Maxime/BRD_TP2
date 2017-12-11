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
