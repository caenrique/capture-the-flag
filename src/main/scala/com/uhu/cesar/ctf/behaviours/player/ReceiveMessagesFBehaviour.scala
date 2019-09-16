package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.domain.{ServerMessage, ServerMessageLine}
import com.uhu.cesar.ctf.domain.ServerMessage.ServerMessage
import com.uhu.cesar.ctf.domain.map.{CTFMap, Player}
import jade.lang.acl.MessageTemplate

trait ReceiveMessagesFBehaviour {

  def receiveMessages: PlayerBehaviour = { agent =>
    (data, aa) =>
      val message = agent.blockingReceive(MessageTemplate.MatchSender(data.server))
      println("next messages: " + message.getContent)

      val newData = if (data.config.partialVision) {
        val firstLine :: rest = message.getContent.split("\n").toList

        (firstLine.split(",").toList match {
          case x :: y :: heading :: xMin :: xMax :: yMin :: yMax :: mapString :: Nil =>
            val p = Player.parseFromString(x, y, heading, data.me.team.id)
            val m = CTFMap.parsePartial(mapString, xMin, xMax, yMin, yMax, data.map.width.toString,
              data.map.height.toString, rest.flatMap(ServerMessageLine.parse))

            for {
              player <- p
              map <- m
            } yield data.updateMap(map).updatePlayer(player)
          case _ => None
        }).getOrElse(data)
      } else {
        data.update(ServerMessage.parse(message.getContent))
      }

      agent.lastMessage = message
      (newData, aa)
  }
}
