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

  val myTeam = 0
  var lastMessage: ACLMessage = _

  override def setup(): Unit = {

    println(s"soy $getName")
    addBehaviour(getConnectionBehaviour(
      connectToServerMessage(myTeam, "CTF_2019", "Team_Cesar"),
      (result, msg) => {

        println(s"$result: $msg")
        val data = CTFState.parse(myTeam, serverAID, msg.getContent)
          .getOrElse(throw new NoGameDataException("unable to parse the data provided by the server"))

        val behaviourList = List(receiveMessages, talkToBoard, listenToTeam, checkPosition, think, sendAction)
          .map(LoopBehaviour.createChildBehaviour(this))

        addBehaviour(new LoopBehaviour(this, data, Nula: AgentAction, behaviourList))
      }
    ))
  }

  def think: PlayerBehaviour = { agent =>
    (data, aa) =>
      val meWithFlag = data.map.flags.exists(f => f.x == data.me.x && f.y == data.me.y && f.team != data.me.team)
      data.teamState match {
        case Attacking =>
          if (meWithFlag) {
            val params = (data.copy(teamState = WithTheFlag), aa)
            changeTeamState(agent).tupled.andThen(goToBase(agent).tupled)(params)
          } else if (data.map.flags.exists(_.team != data.me.team)) {
            takeTheFlag(agent)(data, aa)
          } else {
            explore(agent)(data, aa)
          }
        case WithTheFlag =>
          if (meWithFlag) goToBase(agent)(data, aa)
          else explore(agent)(data, aa)
      }
  }

}
