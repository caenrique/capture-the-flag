package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.domain.ServerMessage
import jade.lang.acl.MessageTemplate

trait ReceiveMessagesFBehaviour {

  def receiveMessages: PlayerBehaviour = { agent =>
    (data, aa) =>
      val message = agent.blockingReceive(MessageTemplate.MatchSender(data.server))
      val serverMessage = ServerMessage.parse(message.getContent)
      val newData = data.update(serverMessage)
      agent.lastMessage = message
      (newData, aa)
  }
}
