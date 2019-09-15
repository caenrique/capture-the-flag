package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.domain.{Board, CTFState, TeamState}
import io.circe.generic.auto._
import io.circe.parser.decode
import jade.lang.acl.{ACLMessage, MessageTemplate}

trait ListenToTeamFBehaviour {

  def listenToTeam: PlayerBehaviour = { agent =>
    (data, aa) =>
      val template = MessageTemplate.and(
        MessageTemplate.MatchSender(data.board.get),
        MessageTemplate.MatchConversationId(Board.teamComunication)
      )
      val maybeNullMsg = agent.receive(template)
      val newState = Option[ACLMessage](maybeNullMsg)
        .flatMap(msg => decode[TeamState](msg.getContent).toOption)
        .getOrElse(data.teamState)

      (CTFState.teamState.set(newState)(data), aa)
  }

}
