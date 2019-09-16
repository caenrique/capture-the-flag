package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.domain.map.CTFMap
import com.uhu.cesar.ctf.domain.{Board, CTFState}
import com.uhu.cesar.ctf.utilities.MessageService
import io.circe.generic.auto._
import io.circe.syntax._
import jade.lang.acl.ACLMessage

trait TalkToBoardFBehaviour extends MessageService {

  def talkToBoard: PlayerBehaviour = { agent =>
    (data, aa) =>
      val newData = if (data.board.isDefined) {
        sendMessage(agent, data.board.get, data.map.asJson.noSpaces, ACLMessage.INFORM)
        data
      } else {
        val who = sendMessage(agent, Board.service(agent.team), data.map.asJson.noSpaces, ACLMessage.INFORM)
        data.copy(board = Some(who))
      }

      val response = receiveJsonResponse[CTFMap](agent, newData.board.get)

      response match {
        case Left(err) => println("PlayerAgent: " + err.getMessage)
        case _ =>
      }

      val map = response.getOrElse(data.map)
      val updated = CTFState.map.set(map)

      (updated(newData), aa)
  }
}
