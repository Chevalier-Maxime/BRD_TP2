package exercice2

import exercice2.ActionMonstre
import exercice2.TypeAction.TypeAction
import org.apache.spark.graphx.{EdgeContext, VertexId}
import scala.collection.mutable.ArrayBuffer

class AngelSolar(
                  var position:Position,
                  equipe:Int,
                  Lvl:Int
                ) extends Monstre(position,"Angel Solar",equipe,100,Lvl) {


  var massHealDisponible: Boolean = true

  var healDisponible: Boolean = true


  override def actionPossible(triplet: EdgeContext[Monstre, EdgeProperty, ArrayBuffer[msg]]) = {
    //Si ennemi
    if(triplet.attr.getRelation() == TypeRelation.ENEMY){
      if(distance(triplet.dstAttr.getPosition()) <= 110){
        val m = new ArrayBuffer[msg]()
        m.append(new msg(TypeAction.ATTAQUE,triplet.dstId,triplet.dstAttr.getPosition()))
        triplet.sendToSrc(m)
      }
      else{
        val m = new ArrayBuffer[msg]()
        m.append(new msg(TypeAction.MOVE,triplet.dstId,triplet.dstAttr.getPosition()))
        triplet.sendToSrc(m)
      }

    }else
      //Si allier
      //TODO tester si l'allier est a portée
      if(triplet.dstAttr.PDV < triplet.dstAttr.getPDVMax() % 0.2){
        val m = new ArrayBuffer[msg]()
        m.append(new msg(TypeAction.HEAL,triplet.dstId,triplet.dstAttr.getPosition()))
        triplet.sendToSrc(m)
    }


    }

  override def choisirAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[msg]) : Monstre = {


    var retttt : Monstre = new AngelSolar(monstres.getPosition(),monstres.getEquipe(),monstres.getLvl())
    /*retttt.setNextAction(vid,TypeAction.MOVE)
    return retttt*/

    var nbDemandeHeal = 0
    var vertexIdAllierEnPLS : VertexId = -1
    //Si demande de Heal
    if(this.healDisponible || this.massHealDisponible){
      //TODO pour le moment on soigne pas le plus blessé
      msgs.foreach(message => message.actionType match {
        case TypeAction.HEAL => nbDemandeHeal+=1
          if(vertexIdAllierEnPLS == -1) vertexIdAllierEnPLS = message.idDest
        case _ =>
      })
      if(nbDemandeHeal == 1){
        retttt.setNextAction(vertexIdAllierEnPLS,TypeAction.HEAL)
        return retttt
      }else if(nbDemandeHeal > 3){
        retttt.setNextAction(-1,TypeAction.ATTAQUE)
        return retttt
      }
    }

    var vertexIdPremierMonstre : VertexId = -1

    def findAttaque {msgs.foreach(message => message.actionType match {
      case TypeAction.ATTAQUE => vertexIdPremierMonstre = message.idDest; return
      case _ =>
    })}

    findAttaque

    if(vertexIdPremierMonstre != -1){
      retttt.setNextAction(vertexIdPremierMonstre,TypeAction.ATTAQUE)
      return retttt
    }

    //Deplacement vers le plus proche
    var vertexIdMechantLePLusProche :VertexId = -1
    var distanceMechantLePlusProche = Double.MaxValue
    msgs.foreach(message => message.actionType match {
      case TypeAction.MOVE =>
        val d = distance(message.posSrc)
        if(d < distanceMechantLePlusProche){
        vertexIdMechantLePLusProche = message.idDest
        distanceMechantLePlusProche = d
      }
      case _ =>
    })

    if(distanceMechantLePlusProche != Double.MaxValue){
      retttt.setNextAction(vertexIdPremierMonstre,TypeAction.MOVE)
    }
    retttt

  }


  def heal() : Unit = {
    this.healDisponible = false
  }
}


class WorgsRider(
                  position:Position,
                  equipe:Int,
                  Lvl:Int
                ) extends Monstre(position,"Worgs Rider",equipe,100,Lvl) {

  override def actionPossible(triplet: EdgeContext[Monstre, EdgeProperty, ArrayBuffer[msg]]) : Unit = {
    //Si ennemi
    if(triplet.attr.getRelation() == TypeRelation.ENEMY){
      if(distance(triplet.dstAttr.getPosition()) <= 20){
        val m = new ArrayBuffer[msg]()
        m.append(msg(TypeAction.ATTAQUE,triplet.dstId,triplet.dstAttr.getPosition()))
        triplet.sendToSrc(m)
      }
      else{
        val m = new ArrayBuffer[msg]()
        m.append(msg(TypeAction.MOVE,triplet.dstId,triplet.dstAttr.getPosition()))
        triplet.sendToSrc(m)
      }
    }
  }

  override def choisirAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[msg]) : Monstre = {

    var retttt : Monstre = new BarbareOrc(monstres.getPosition(),monstres.getEquipe(),monstres.getLvl())
    /*retttt.setNextAction(vid,TypeAction.MOVE)
    return retttt*/

    var vertexIdPremierMonstre : VertexId = -1
    def findAttaque {msgs.foreach(message => message.actionType match {
      case TypeAction.ATTAQUE => vertexIdPremierMonstre = message.idDest; return
      case _ =>
    })}

    findAttaque
    if(vertexIdPremierMonstre != -1){
      retttt.setNextAction(vertexIdPremierMonstre,TypeAction.ATTAQUE)
      return retttt
    }

    //Deplacement vers le plus proche
    var vertexIdMechantLePLusProche :VertexId = -1
    var distanceMechantLePlusProche = Double.MaxValue
    msgs.foreach(message => message.actionType match {
      case TypeAction.MOVE =>
        val d = distance(message.posSrc)
        if(d < distanceMechantLePlusProche){
          vertexIdMechantLePLusProche = message.idDest
          distanceMechantLePlusProche = d
        }
      case _ =>
    })

    if(distanceMechantLePlusProche != Double.MaxValue){
      retttt.setNextAction(vertexIdMechantLePLusProche,TypeAction.MOVE)
    }
    retttt
  }
}


class LeWarlord(
                  position:Position,
                  equipe:Int,
                  Lvl:Int
                ) extends Monstre(position,"Le Warlord",equipe,100,Lvl) {


  override def actionPossible(triplet: EdgeContext[Monstre, EdgeProperty, ArrayBuffer[msg]]) = {
    //Si ennemi
    if(triplet.attr.getRelation() == TypeRelation.ENEMY){
      if(distance(triplet.dstAttr.getPosition()) <= 20){
        val m = new ArrayBuffer[msg]()
        m.append(new msg(TypeAction.ATTAQUE,triplet.dstId,triplet.dstAttr.getPosition()))
        triplet.sendToSrc(m)
      }
      else{
        val m = new ArrayBuffer[msg]()
        m.append(new msg(TypeAction.MOVE,triplet.dstId,triplet.dstAttr.getPosition()))
        triplet.sendToSrc(m)
      }
    }
  }

  override def choisirAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[msg]) : Monstre = {

    var retttt : Monstre = new BarbareOrc(monstres.getPosition(),monstres.getEquipe(),monstres.getLvl())
    /*retttt.setNextAction(vid,TypeAction.MOVE)
    return retttt*/

    var vertexIdPremierMonstre : VertexId = -1
    def findAttaque {msgs.foreach(message => message.actionType match {
      case TypeAction.ATTAQUE => vertexIdPremierMonstre = message.idDest; return
      case _ =>
    })}

    findAttaque
    if(vertexIdPremierMonstre != -1){
      retttt.setNextAction(vertexIdPremierMonstre,TypeAction.ATTAQUE)
      return retttt
    }

    //Deplacement vers le plus proche
    var vertexIdMechantLePLusProche :VertexId = -1
    var distanceMechantLePlusProche = Double.MaxValue
    msgs.foreach(message => message.actionType match {
      case TypeAction.MOVE =>
        val d = distance(message.posSrc)
        if(d < distanceMechantLePlusProche){
          vertexIdMechantLePLusProche = message.idDest
          distanceMechantLePlusProche = d
        }
      case _ =>
    })

    if(distanceMechantLePlusProche != Double.MaxValue){
      retttt.setNextAction(vertexIdMechantLePLusProche,TypeAction.MOVE)
    }
    retttt
  }
}

class BarbareOrc(
                 position:Position,
                 equipe:Int,
                 Lvl:Int
               ) extends Monstre(position,"Barbare Orc",equipe,100,Lvl) {


  override def actionPossible(triplet: EdgeContext[Monstre, EdgeProperty, ArrayBuffer[msg]]) = {
    //Si ennemi
    if(triplet.attr.getRelation() == TypeRelation.ENEMY){
      if(distance(triplet.dstAttr.getPosition()) <= 20){
        val m = new ArrayBuffer[msg]()
        m.append(new msg(TypeAction.ATTAQUE,triplet.dstId,triplet.dstAttr.getPosition()))
        triplet.sendToSrc(m)
      }
      else{
        val m = new ArrayBuffer[msg]()
        m.append(new msg(TypeAction.MOVE,triplet.dstId,triplet.dstAttr.getPosition()))
        triplet.sendToSrc(m)
      }
    }
  }

  override def choisirAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[msg]) : Monstre = {

    var retttt : Monstre = new BarbareOrc(monstres.getPosition(),monstres.getEquipe(),monstres.getLvl())
    /*retttt.setNextAction(vid,TypeAction.MOVE)
    return retttt*/

    var vertexIdPremierMonstre : VertexId = -1
    def findAttaque {msgs.foreach(message => message.actionType match {
      case TypeAction.ATTAQUE => vertexIdPremierMonstre = message.idDest; return
      case _ =>
    })}

    findAttaque
    if(vertexIdPremierMonstre != -1){
      retttt.setNextAction(vertexIdPremierMonstre,TypeAction.ATTAQUE)
      return retttt
    }

    //Deplacement vers le plus proche
    var vertexIdMechantLePLusProche :VertexId = -1
    var distanceMechantLePlusProche = Double.MaxValue
    msgs.foreach(message => message.actionType match {
      case TypeAction.MOVE =>
        val d = distance(message.posSrc)
        if(d < distanceMechantLePlusProche){
          vertexIdMechantLePLusProche = message.idDest
          distanceMechantLePlusProche = d
        }
      case _ =>
    })

    if(distanceMechantLePlusProche != Double.MaxValue){
      retttt.setNextAction(vertexIdMechantLePLusProche,TypeAction.MOVE)
    }
    retttt
  }
}