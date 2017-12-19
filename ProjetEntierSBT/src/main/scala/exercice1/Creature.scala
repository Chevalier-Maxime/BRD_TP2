package exercice1

/*import net.liftweb.json.JsonDSL._
import net.liftweb.json._*/

class Creature(val name: String) extends Serializable {
  var spells = scala.collection.mutable.Set[String]()

  def addspell(spell: String): Unit = {
    spells += spell
  }

  /*def toJson():JObject = {
    val json =
      ( //"creature" ->
        ("name" -> name) ~
        ("spells" -> spells)
      )


    return json //compact(render(json))
  }*/
}
