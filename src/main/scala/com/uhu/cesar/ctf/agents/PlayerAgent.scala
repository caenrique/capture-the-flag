package com.uhu.cesar.ctf.agents

import com.uhu.cesar.ctf.behaviours.LoopBehaviour
import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours
import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.domain.AgentAction.Nula
import com.uhu.cesar.ctf.domain.TeamState.{Attacking, WithTheFlag}
import com.uhu.cesar.ctf.domain._
import com.uhu.cesar.ctf.domain.exceptions.NoGameDataException
import com.uhu.cesar.ctf.utilities.ServerConnectionService
import jade.core.Agent
import jade.lang.acl.ACLMessage

class PlayerAgent extends Agent with ServerConnectionService with PlayerBehaviours {

  var team: Int = _
  var lastMessage: ACLMessage = _

  override def setup(): Unit = {

    val team0 = getArguments.headOption.flatMap(_.asInstanceOf[String].toIntOption)

    if (team0.isDefined) {
      team = team0.get
    } else {
      println("FALLO AL PARSEAR EL EQUIPO")
      team = 0
    }

    addBehaviour(getConnectionBehaviour(
      connectToServerMessage(team, "CTF_2019", "Team_Cesar"),
      (result, msg) => {

        val data = CTFState.parse(team, serverAID, msg.getContent)
          .getOrElse(throw new NoGameDataException("unable to parse the data provided by the server"))

        val behaviourList = List(receiveMessages, talkToBoard, listenToTeam, checkPosition, think, dodgePlayer, sendAction)
          .map(LoopBehaviour.createChildBehaviour(this))

        addBehaviour(new LoopBehaviour(this, data, Nula: AgentAction, behaviourList))
      }
    ))
  }

  def think: PlayerBehaviour = { agent =>
    (data, aa) =>

      val meWithFlag = data.map.flags.exists(f => f.x == data.me.x && f.y == data.me.y)

      data.teamState match {

        case Attacking =>
          if (meWithFlag) toState(WithTheFlag, goToBase)(agent)(data, aa)
          else if (data.map.flags.exists(_.team != data.me.team)) {
            val newData = if (data.exploring) data.copy(exploring = false, computePath = true) else data
            takeTheFlag(agent)(newData, aa)
          } else {
            val newData = if (data.exploring) data else data.copy(exploring = true, computePath = true)
            explore(agent)(newData, aa)
          }

        case WithTheFlag =>
          if (meWithFlag) goToBase(agent)(data, aa)
          else takeDownEnemy(agent)(data, aa)
      }
  }

  def toState(state: TeamState, nextBehabiour: PlayerBehaviour): PlayerBehaviour = { agent =>
    (data, aa) =>
      val params = (data.copy(teamState = state, computePath = true), aa)
      changeTeamState(agent).tupled.andThen(nextBehabiour(agent).tupled)(params)
  }

}
