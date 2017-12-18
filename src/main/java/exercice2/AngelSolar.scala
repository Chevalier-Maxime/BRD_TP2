package exercice2

import exercice2.ActionMonstre
import exercice2.TypeAction.TypeAction
import org.apache.spark.graphx.{EdgeContext, VertexId}
import scala.collection.mutable.ArrayBuffer

class AngelSolar(
                       position:Position,
                       equipe:Int,
                       Lvl:Int,
                       PDV:Int
                ) extends Monstre(position,"Angel Solar",equipe,100,Lvl,150,44,PDV) {


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
      if(triplet.dstAttr.getPDV() < triplet.dstAttr.getPDVMax() % 0.2){
        val m = new ArrayBuffer[msg]()
        m.append(new msg(TypeAction.HEAL,triplet.dstId,triplet.dstAttr.getPosition()))
        triplet.sendToSrc(m)
    }


    }

  override def choisirAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[msg]) : Monstre = {


    var retttt : Monstre = new AngelSolar(monstres.getPosition(),monstres.getEquipe(),monstres.getLvl(),monstres.getPDV())
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
      if(nbDemandeHeal == 1 && this.healDisponible){
        retttt.setNextAction(vertexIdAllierEnPLS,TypeAction.HEAL)
        return retttt
      }else if(nbDemandeHeal > 3 && this.massHealDisponible){
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

  def massHeal() : Unit = {
    this.massHealDisponible = false
  }

  override def executeAction(triplet: EdgeContext[Monstre, EdgeProperty, ArrayBuffer[message2]]): Unit  = {

    //var (id, actionType) = this.nextAction
    if(triplet.dstId == nextAction.vertexId){
      println(this + " VID Enregistre = " + triplet.dstId + ", cible : "+nextAction.vertexId)
      nextAction.typeAction match {
        case TypeAction.HEAL => println( triplet.srcId + " heal " + triplet.dstId)
          val m = new ArrayBuffer[message2]()
          //m.append(new msg(TypeAction.HEAL,triplet.srcId,triplet.dstAttr.getPosition()));
          m.append(new  heal(triplet.srcAttr.getLvl(),10))
          heal()
          triplet.sendToDst(m)
        case TypeAction.MOVE => println(triplet.srcId + " se deplace vers " + triplet.dstId)
          val m = new ArrayBuffer[message2]()
          m.append(new deplacement(this.calculDeplacement(triplet.dstAttr.getPosition(),this.getDeplacementParTour)))
          triplet.sendToSrc(m)
        case TypeAction.ATTAQUE => println(triplet.srcId + " attaque " + triplet.dstId)
          var toucher = 0
          var degats = 0
          if (distance(triplet.dstAttr.position)<=10){
            toucher = d20()+35
            degats = 3*d6()+18
            if (triplet.dstAttr.getArmure() <= toucher ){
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats));
              triplet.sendToDst(m)
            }
            toucher = d20()+30
            degats = 3*d6()+18
            if (triplet.dstAttr.getArmure() <= toucher ){
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats));
              triplet.sendToDst(m)
            }
            toucher = d20()+25
            degats = 3*d6()+18
            if (triplet.dstAttr.getArmure() <= toucher ){
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats));
              triplet.sendToDst(m)
            }
            toucher = d20()+20
            degats = 3*d6()+18
            if (triplet.dstAttr.getArmure() <= toucher ){
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats));
              triplet.sendToDst(m)
            }
          }else{
            toucher = d20()+31
            degats = 2*d6()+14
            if (triplet.dstAttr.getArmure() <= toucher ){
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats));
              triplet.sendToDst(m)
            }
            toucher = d20()+26
            degats = 3*d6()+18
            if (triplet.dstAttr.getArmure() <= toucher ){
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats));
              triplet.sendToDst(m)
            }
            toucher = d20()+21
            degats = 3*d6()+18
            if (triplet.dstAttr.getArmure() <= toucher ){
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats));
              triplet.sendToDst(m)
            }
            toucher = d20()+16
            degats = 3*d6()+18
            if (triplet.dstAttr.getArmure() <= toucher ){
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats));
              triplet.sendToDst(m)
            }
          }
      }
      //MASS HEAL
    }else if(nextAction.vertexId == -1 && triplet.attr.getRelation() == TypeRelation.FRIEND){
      massHeal()
      val m = new ArrayBuffer[message2]()
      //m.append(new msg(TypeAction.HEAL,triplet.srcId,triplet.dstAttr.getPosition()));
      m.append(new  heal(triplet.srcAttr.getLvl(),10))
      triplet.sendToDst(m)
    }
  }

  override def receptionnerAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[message2]): _root_.exercice2.Monstre = {
    var retttt : Monstre = new AngelSolar(monstres.getPosition(),monstres.getEquipe(),monstres.getLvl())
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
}


class WorgsRider(
                  position:Position,
                  equipe:Int,
                  Lvl:Int,
                  PDV:Int
                ) extends Monstre(position,"Worgs Rider",equipe,100,Lvl,60,18,PDV) {

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

  override def receptionnerAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[message2]): _root_.exercice2.Monstre = {
    var retttt : Monstre = new WorgsRider(monstres.getPosition(),monstres.getEquipe(),monstres.getLvl())
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
      println(this + " VID Enregistre = " + triplet.dstId + ", cible : " + nextAction.vertexId)
      nextAction.typeAction match {
        case TypeAction.MOVE => println(triplet.srcId + " se deplace vers " + triplet.dstId)
          val m = new ArrayBuffer[message2]()
          m.append(new deplacement(this.calculDeplacement(triplet.dstAttr.getPosition(), this.getDeplacementParTour)))
          triplet.sendToSrc(m)
        case TypeAction.ATTAQUE => println(triplet.srcId + " attaque " + triplet.dstId)
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
          }
      }
    }
  }
}


class LeWarlord(
                  position:Position,
                  equipe:Int,
                  Lvl:Int,
                  PDV:Int
                ) extends Monstre(position,"Le Warlord",equipe,100,Lvl,90,27,PDV) {


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

  override def receptionnerAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[message2]): _root_.exercice2.Monstre = {
    var retttt : Monstre = new LeWarlord(monstres.getPosition(),monstres.getEquipe(),monstres.getLvl())
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

  override def choisirAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[msg]) : Monstre = {

    var retttt : Monstre = new LeWarlord(monstres.getPosition(),monstres.getEquipe(),monstres.getLvl(),monstres.getPDV()
    )
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
  override def executeAction(triplet: EdgeContext[Monstre, EdgeProperty, ArrayBuffer[message2]]): Unit = {

    //var (id, actionType) = this.nextAction
    if (triplet.dstId == nextAction.vertexId) {
      println(this + " VID Enregistre = " + triplet.dstId + ", cible : " + nextAction.vertexId)
      nextAction.typeAction match {
        case TypeAction.MOVE => println(triplet.srcId + " se deplace vers " + triplet.dstId)
          val m = new ArrayBuffer[message2]()
          m.append(new deplacement(this.calculDeplacement(triplet.dstAttr.getPosition(), this.getDeplacementParTour)))
          triplet.sendToSrc(m)
        case TypeAction.ATTAQUE => println(triplet.srcId + " attaque " + triplet.dstId)
          var toucher = 0
          var degats = 0
          if (distance(triplet.dstAttr.position) <= 10) {
            toucher = d20() + 20
            degats = d8() + 10
            if (triplet.dstAttr.getArmure() <= toucher) {
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats));
              triplet.sendToDst(m)
            }
            toucher = d20() + 15
            degats = d8() + 10
            if (triplet.dstAttr.getArmure() <= toucher) {
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats));
              triplet.sendToDst(m)
            }
            toucher = d20() + 10
            degats = d8() + 10
            if (triplet.dstAttr.getArmure() <= toucher) {
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats));
              triplet.sendToDst(m)
            }
          } else {
            toucher = d20()+ 19
            degats = d6()+5
            if (triplet.dstAttr.getArmure() <= toucher) {
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats));
              triplet.sendToDst(m)
            }
          }
      }
    }
  }
}

class BarbareOrc(
                 position:Position,
                 equipe:Int,
                 Lvl:Int,
                 PDV:Int
               ) extends Monstre(position,"Barbare Orc",equipe,100,Lvl,120,17,PDV) {

  override def receptionnerAction(vid: VertexId, monstres: Monstre, msgs: ArrayBuffer[message2]): _root_.exercice2.Monstre = {
    var retttt : Monstre = new BarbareOrc(monstres.getPosition(),monstres.getEquipe(),monstres.getLvl())
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

    var retttt : Monstre = new BarbareOrc(monstres.getPosition(),monstres.getEquipe(),monstres.getLvl(),monstres.getPDV())
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
  override def executeAction(triplet: EdgeContext[Monstre, EdgeProperty, ArrayBuffer[message2]]): Unit = {

    //var (id, actionType) = this.nextAction
    if (triplet.dstId == nextAction.vertexId) {
      println(this + " VID Enregistre = " + triplet.dstId + ", cible : " + nextAction.vertexId)
      nextAction.typeAction match {
        case TypeAction.MOVE => println(triplet.srcId + " se deplace vers " + triplet.dstId)
          val m = new ArrayBuffer[message2]()
          m.append(new deplacement(this.calculDeplacement(triplet.dstAttr.getPosition(), this.getDeplacementParTour)))
          triplet.sendToSrc(m)
        case TypeAction.ATTAQUE => println(triplet.srcId + " attaque " + triplet.dstId)
          var toucher = 0
          var degats = 0
          if (distance(triplet.dstAttr.position) <= 10) {
            toucher = d20() + 19
            degats = d8() + 10
            if (triplet.dstAttr.getArmure() <= toucher) {
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats))
              triplet.sendToDst(m)
            }
            toucher = d20() + 14
            degats = d8() + 10
            if (triplet.dstAttr.getArmure() <= toucher) {
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats))
              triplet.sendToDst(m)
            }
            toucher = d20() + 9
            degats = d8() + 10
            if (triplet.dstAttr.getArmure() <= toucher) {
              val m = new ArrayBuffer[message2]()
              m.append(new attaque( degats))
              triplet.sendToDst(m)
            }
          } else {
            toucher = d20()+ 16
            degats = d8() + 6
            if (triplet.dstAttr.getArmure() <= toucher) {
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats))
              triplet.sendToDst(m)
            }
            toucher = d20()+ 11
            degats = d8() + 6
            if (triplet.dstAttr.getArmure() <= toucher) {
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats))
              triplet.sendToDst(m)
            }
            toucher = d20()+ 6
            degats = d8() + 6
            if (triplet.dstAttr.getArmure() <= toucher) {
              val m = new ArrayBuffer[message2]()
              m.append(new attaque(degats))
              triplet.sendToDst(m)
            }
          }
      }
    }
  }
}