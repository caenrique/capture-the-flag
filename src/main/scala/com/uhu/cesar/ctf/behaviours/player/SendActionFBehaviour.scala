package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour

trait SendActionFBehaviour extends {

  def sendAction: PlayerBehaviour = { agent =>
    (data, aa) =>
      val reply = agent.lastMessage.createReply()
      reply.setSender(agent.getAID)
      reply.setContent(aa.toString.toUpperCase)
      agent.send(reply)
      (data, aa)
  }

}
