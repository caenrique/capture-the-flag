package com.uhu.cesar.ctf.agents

import com.uhu.cesar.ctf.algorithms.GoTo
import com.uhu.cesar.ctf.algorithms.GoTo.Point
import com.uhu.cesar.ctf.behaviours.LoopBehaviour
import com.uhu.cesar.ctf.behaviours.LoopBehaviour.BehaviourFunction
import com.uhu.cesar.ctf.domain.AgentAction.{Adelante, Nula, Rotar}
import com.uhu.cesar.ctf.domain.exceptions.NoGameDataException
import com.uhu.cesar.ctf.domain.map.Team
import com.uhu.cesar.ctf.domain.{AgentAction, CTFState, ServerMessage}
import com.uhu.cesar.ctf.utilities.{DFRegister, MessageService, ServerConnectionService}
import jade.core.Agent
import jade.lang.acl.MessageTemplate
import sun.management.resources.agent

class PlayerAgent extends Agent with DFRegister with ServerConnectionService with MessageService {

  val myTeam = 0

  override def setup(): Unit = {

    addBehaviour(getConnectionBehaviour(
      connectToServerMessage(myTeam, "CTF_2019", "Team_Cesar"),
      (result, msg) => {

        println(msg)

        // Parse msg and create CTFMap structure
        val data = CTFState.parse(myTeam, msg.getContent)
          .getOrElse(throw new NoGameDataException("unable to parse the data provided by the server"))
        // Figure out how to move it arrownd so it doesn't become a mutable field: FAILED

        println(data)

        val behaviourList = List(receiveMessages, takeTheFlag, sendAction)
          .map(LoopBehaviour.createChildBehaviour(this))

        addBehaviour(new LoopBehaviour(this, data, Nula: AgentAction, behaviourList))
      }
    ))
  }

  def receiveMessages: BehaviourFunction[PlayerAgent, CTFState, AgentAction] = { agent => (data, aa) =>
      val message = agent.blockingReceive(MessageTemplate.MatchSender(serverAID))
      val serverMessage = ServerMessage.parse(message.getContent)
      val newData = data.update(serverMessage)
      (newData.copy(lastMessage = Some(message)), aa)
  }

  def talkToBoard: BehaviourFunction[PlayerAgent, CTFState, AgentAction] = { Agent => (data, aa) =>
    //sendMessage(BoardAgent.service, )
    ???
  }

  def randomAction: BehaviourFunction[PlayerAgent, CTFState, AgentAction] = { _ => (data, aa) =>
      (data, Adelante)
  }

  def sendAction: BehaviourFunction[PlayerAgent, CTFState, AgentAction] = { agent => (data, aa) =>
      val reply = data.lastMessage.get.createReply()
      reply.setSender(getAID)
      reply.setContent(aa.toString.toUpperCase)
      agent.send(reply)
      (data, aa)
  }

  def takeTheFlag: BehaviourFunction[PlayerAgent, CTFState, AgentAction] = { agent => (data, aa) =>

    val from = (Point(data.me.x, data.me.y), data.me.heading)
    val enemyFlag = data.map.flags.find(_.team != data.me.team).get // Asumimos de momento que la bandera se puede ver
    val nextAction :: newActions = if (data.computePath) {
      GoTo(data)(from, Point(enemyFlag.x, enemyFlag.y))
    } else if (data.actionList.nonEmpty) {
      data.actionList
    } else {
      if(enemyFlag.x == data.me.x && enemyFlag.y == data.me.y) {
        val myBase = data.map.bases.find(_.team == data.me.team).get
        GoTo(data)(from, Point(myBase.x, myBase.y))
      } else Nula :: Nil
    }

    if (data.me == data.lastPosition && aa != Nula) {
      (data.copy(computePath = false, actionList = nextAction :: newActions), aa)
    } else {
      (data.copy(computePath = false, actionList = newActions), nextAction)
    }
  }

  override def takeDown() = deRegister
}
