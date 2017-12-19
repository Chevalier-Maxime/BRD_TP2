package exercice2.Monstres

import exercice2._
import org.apache.spark.graphx.{EdgeContext, VertexId}

import scala.collection.mutable.ArrayBuffer

class WorgsRider(
                  position:Position,
                  equipe:Int,
                  Lvl:Int,
                  PDV:Int
                ) extends Monstre(position,"Worgs Rider",equipe,13,Lvl,60,18,PDV) {

  override def actionPossible(triplet: EdgeContext[Monstre, EdgeProperty, ArrayBuffer[msg]]): Unit = {
    //Si ennemi
    if (triplet.attr.getRelation() == TypeRelation.ENEMY) {
      if (distance(triplet.dstAttr.getPosition()) <= 20) {
        val m = new ArrayBuffer[msg]()
        m.append(msg(TypeAction.ATTAQUE, triplet.dstId, triplet.dstAttr.getPosition()))
        triplet.sendToSrc(m)
      }
      else {
        val m = new ArrayBuffer[msg]()
        m.append(msg(TypeAction.MOVE, triplet.dstId, triplet.dstAttr.getPosition()))
        triplet.sendToSrc(m)
      }
    }
  }

  override def receptionnerAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[message2]): Monstre = {
    var retttt : Monstre = new WorgsRider(monstres.getPosition(),monstres.getEquipe(),monstres.getLvl(),monstres.getPDV())
    var messagePrint = "Moi "+monstres.getNom()+"@"+vid+" recoit les differentes actions :"
    msgs.foreach(message => message.getActionType match {
      case TypeAction.MOVE => messagePrint += "MOVE ";
        val messageDepl = message.asInstanceOf[deplacement]
        retttt.setPosition(messageDepl.getPosition)
      case TypeAction.ATTAQUE => messagePrint +="ATTAQUE "
        val messageAttaque = message.asInstanceOf[attaque]
        retttt.removePDV(messageAttaque.degats)
      case TypeAction.HEAL => messagePrint += "SOIN"
        val messageHeal = message.asInstanceOf[heal]
        retttt.addPDV(messageHeal.Lvl*messageHeal.multiplicateur)
    })

    println(messagePrint)

    //this.nextAction = null
    retttt
  }

  override def choisirAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[msg]): Monstre = {

    var retttt : Monstre = new WorgsRider(monstres.getPosition(),monstres.getEquipe(),monstres.getLvl(),monstres.getPDV())
    /*retttt.setNextAction(vid,TypeAction.MOVE)
    return retttt*/

    var vertexIdPremierMonstre: VertexId = -1

    def findAttaque {
      msgs.foreach(message => message.actionType match {
        case TypeAction.ATTAQUE => vertexIdPremierMonstre = message.idDest; return
        case _ =>
      })
    }

    findAttaque
    if (vertexIdPremierMonstre != -1) {
      retttt.setNextAction(vertexIdPremierMonstre, TypeAction.ATTAQUE)
      return retttt
    }

    //Deplacement vers le plus proche
    var vertexIdMechantLePLusProche: VertexId = -1
    var distanceMechantLePlusProche = Double.MaxValue
    msgs.foreach(message => message.actionType match {
      case TypeAction.MOVE =>
        val d = distance(message.posSrc)
        if (d < distanceMechantLePlusProche) {
          vertexIdMechantLePLusProche = message.idDest
          distanceMechantLePlusProche = d
        }
      case _ =>
    })

    if (distanceMechantLePlusProche != Double.MaxValue) {
      retttt.setNextAction(vertexIdMechantLePLusProche, TypeAction.MOVE)
    }
    retttt
  }

  override def executeAction(triplet: EdgeContext[Monstre, EdgeProperty, ArrayBuffer[message2]]): Unit = {

    //var (id, actionType) = this.nextAction
    if (triplet.dstId == nextAction.vertexId) {
      //println(this + " VID Enregistre = " + triplet.dstId + ", cible : " + nextAction.vertexId)
      nextAction.typeAction match {
        case TypeAction.MOVE => println("WorgsRider " +triplet.srcId + " se deplace vers " + triplet.dstAttr.getNom()+ triplet.dstId)
          val m = new ArrayBuffer[message2]()
          m.append(new deplacement(this.calculDeplacement(triplet.dstAttr.getPosition(), this.getDeplacementParTour)))
          triplet.sendToSrc(m)
        case TypeAction.ATTAQUE =>
          var toucher = 0
          var degats = 0
          if (distance(triplet.dstAttr.position) <= 10) {
            toucher = d20()+6
            degats = (d8() + 2) * 3
          } else {
            toucher = d20()+4
            degats = d6() * 3
          }
          if (triplet.dstAttr.getArmure() <= toucher) {
            val m = new ArrayBuffer[message2]()
            m.append(new attaque( degats));
            triplet.sendToDst(m)
            println("WorgsRider " +triplet.srcId + " attaque " + triplet.dstAttr.getNom()+ triplet.dstId)
          }
          else {
            println("WorgsRider " +triplet.srcId + " rate son attaque contre  "+ triplet.dstAttr.getNom() + triplet.dstId)
          }
      }
    }
  }
}
