package com.uhu.cesar.ctf.agents

import com.uhu.cesar.ctf.algorithms.GoTo
import com.uhu.cesar.ctf.algorithms.GoTo.Point
import com.uhu.cesar.ctf.behaviours.LoopBehaviour
import com.uhu.cesar.ctf.behaviours.LoopBehaviour.BehaviourFunction
import com.uhu.cesar.ctf.domain.AgentAction.{Adelante, Nula}
import com.uhu.cesar.ctf.domain.exceptions.NoGameDataException
import com.uhu.cesar.ctf.domain.map.CTFMap
import com.uhu.cesar.ctf.domain.{AgentAction, Board, CTFState, ServerMessage}
import com.uhu.cesar.ctf.utilities.{DFRegister, MessageService, ServerConnectionService}
import jade.core.Agent
import jade.lang.acl.{ACLMessage, MessageTemplate}
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe._

class PlayerAgent extends Agent with ServerConnectionService with MessageService {

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

        val behaviourList = List(receiveMessages, talkToBoard, takeTheFlag, sendAction)
          .map(LoopBehaviour.createChildBehaviour(this))

        addBehaviour(new LoopBehaviour(this, data, Nula: AgentAction, behaviourList))
      }
    ))
  }

  def receiveMessages: BehaviourFunction[PlayerAgent, CTFState, AgentAction] = { agent =>
    (data, aa) =>
      val message = agent.blockingReceive(MessageTemplate.MatchSender(serverAID))
      val serverMessage = ServerMessage.parse(message.getContent)
      val newData = data.update(serverMessage)
      lastMessage = message
      (newData, aa)
  }

  def sendAction: BehaviourFunction[PlayerAgent, CTFState, AgentAction] = { agent =>
    (data, aa) =>
      val reply = lastMessage.createReply()
      reply.setSender(getAID)
      reply.setContent(aa.toString.toUpperCase)
      agent.send(reply)
      (data, aa)
  }

  def takeTheFlag: BehaviourFunction[PlayerAgent, CTFState, AgentAction] = { agent =>
    (data, aa) =>

      val from = (Point(data.me.x, data.me.y), data.me.heading)
      val enemyFlag = data.map.flags.find(_.team != data.me.team).get // Asumimos de momento que la bandera se puede ver

      // TODO: Reacer esto bien con estados para saber si se está buscando la bandera o ya la tiene
      val nextAction :: newActions = if (data.computePath) {
        GoTo(data)(from, Point(enemyFlag.x, enemyFlag.y))
      } else if (data.actionList.nonEmpty) {
        data.actionList
      } else {
        if (enemyFlag.x == data.me.x && enemyFlag.y == data.me.y) {
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

  def talkToBoard: BehaviourFunction[PlayerAgent, CTFState, AgentAction] = { Agent => (data, aa) =>
    val who = sendMessage(Board.service, data.map.asJson.noSpaces, ACLMessage.INFORM)
    val response = receiveJsonResponse[CTFMap](who)

    response match {
      case Left(err) => println("PlayerAgent: " + err.getMessage)
      case _ =>
    }

    val map = response.getOrElse(data.map)

    (CTFState.map.set(map)(data), aa)
  }

  def randomAction: BehaviourFunction[PlayerAgent, CTFState, AgentAction] = { _ =>
    (data, aa) =>
      (data, Adelante)
  }
}
