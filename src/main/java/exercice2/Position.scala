package exercice2

class Position(var x: Int, var y:Int) extends Serializable {

  def maj(x:Int, y:Int): Unit = {
    this.x = x;
    this.y = y
  }
}
