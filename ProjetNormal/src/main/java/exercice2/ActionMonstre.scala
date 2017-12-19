package exercice2

import exercice2.Monstres.Monstre

abstract class ActionMonstre(nom:String) extends Serializable{
  def executerAction(monstre: Monstre)

}
