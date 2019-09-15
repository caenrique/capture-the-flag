package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.utilities.MessageService
import io.circe.generic.auto._
import io.circe.syntax._
import jade.lang.acl.ACLMessage

trait ChangeTeamStateFBehaviour extends MessageService {

  def changeTeamState: PlayerBehaviour = { agent =>
    (data, aa) =>
      val newState = data.teamState
      sendMessage(agent, data.board.get, newState.asJson.noSpaces, ACLMessage.PROPAGATE)
      println(s"Cambio de estado: $newState")
      (data.copy(computePath = true), aa)
  }

}
