package exercice2

import exercice2.TypeRelation.TypeRelation

object TypeRelation extends Enumeration with Serializable {
  type TypeRelation = Value
  val FRIEND, ENEMY = Value
}
class EdgeProperty(relation: TypeRelation) extends Serializable {

  def getRelation():TypeRelation ={
    return  relation
  }

}
