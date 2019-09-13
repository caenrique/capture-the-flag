package com.uhu.cesar.ctf.agents

import com.uhu.cesar.ctf.algorithms.GoTo
import com.uhu.cesar.ctf.algorithms.GoTo.Point
import com.uhu.cesar.ctf.behaviours.LoopBehaviour
import com.uhu.cesar.ctf.behaviours.LoopBehaviour.BehaviourFunction
import com.uhu.cesar.ctf.domain.AgentAction.{Adelante, Nula, Rotar}
import com.uhu.cesar.ctf.domain.exceptions.NoGameDataException
import com.uhu.cesar.ctf.domain.map.Team
import com.uhu.cesar.ctf.domain.{AgentAction, CTFGameData, ServerMessage}
import com.uhu.cesar.ctf.utilities.{DFRegister, ServerConnectionService}
import jade.core.Agent
import jade.lang.acl.MessageTemplate

class HelloAgent extends Agent with DFRegister with ServerConnectionService {

  val myTeam = 0

  override def setup(): Unit = {

    addBehaviour(getConnectionBehaviour(
      connectToServerMessage(myTeam, "CTF_2019", "Team_Cesar"),
      (result, msg) => {

        println(msg)

        // Parse msg and create CTFMap structure
        val data = CTFGameData.parse(myTeam, msg.getContent)
          .getOrElse(throw new NoGameDataException("unable to parse the data provided by the server"))
        // Figure out how to move it arrownd so it doesn't become a mutable field: FAILED

        println(data)

        val behaviourList = List(receiveMessages, takeTheFlag, sendAction)
          .map(LoopBehaviour.createChildBehaviour(this))

        addBehaviour(new LoopBehaviour(this, data, Nula: AgentAction, behaviourList))
      }
    ))
  }

  def receiveMessages: BehaviourFunction[HelloAgent, CTFGameData, AgentAction] = { agent => (gd, aa) =>
      val message = agent.blockingReceive(MessageTemplate.MatchSender(serverAID))
      val serverMessage = ServerMessage.parse(message.getContent)
      val newData = gd.update(serverMessage)
      (newData.copy(lastMessage = Some(message)), aa)
  }

  def randomAction: BehaviourFunction[HelloAgent, CTFGameData, AgentAction] = { _ => (gd, aa) =>
      (gd, Adelante)
  }

  def sendAction: BehaviourFunction[HelloAgent, CTFGameData, AgentAction] = { agent => (gd, aa) =>
      val reply = gd.lastMessage.get.createReply()
      reply.setSender(getAID)
      reply.setContent(aa.toString.toUpperCase)
      agent.send(reply)
      (gd, aa)
  }

  def takeTheFlag: BehaviourFunction[HelloAgent, CTFGameData, AgentAction] = { agent => (data, aa) =>

    val from = (Point(data.me.x, data.me.y), data.me.heading)
    val enemyFlag = if (data.me.team == Team.RED) data.blueFlag else data.redFlag
    val nextAction :: newActions = if (data.computePath) {
      GoTo(data)(from, Point(enemyFlag.x, enemyFlag.y))
    } else if (data.actionList.nonEmpty) {
      data.actionList
    } else {
      Nula :: Nil
    }

    if (data.me == data.lastPosition && aa != Nula) {
      (data.copy(computePath = false, actionList = nextAction :: newActions), aa)
    } else {
      (data.copy(computePath = false, actionList = newActions), nextAction)
    }
  }

  override def takeDown() = deRegister
}
