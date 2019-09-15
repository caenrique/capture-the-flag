package com.uhu.cesar.ctf.agents

import com.uhu.cesar.ctf.behaviours.LoopBehaviour
import com.uhu.cesar.ctf.behaviours.LoopBehaviour.BehaviourFunction
import com.uhu.cesar.ctf.domain.Board
import com.uhu.cesar.ctf.domain.Board.{Response, State}
import com.uhu.cesar.ctf.domain.map.CTFMap
import com.uhu.cesar.ctf.utilities.DFRegister
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import jade.core.{AID, Agent}
import jade.lang.acl.{ACLMessage, MessageTemplate}

class BoardAgent extends Agent with DFRegister {

  override def setup(): Unit = {
    registerService(Board.service)

    val behaviours = List(receiveMessage, sendResponse)

    addBehaviour(
      new LoopBehaviour(
        this,
        Board.emptyState,
        Response(getAID, None),
        behaviours.map(LoopBehaviour.createChildBehaviour[BoardAgent, State, Response](this))
      )
    )

  }

  def receiveMessage: BehaviourFunction[BoardAgent, State, Response] = { agent => (data, _) =>

      val message: ACLMessage = agent.blockingReceive(MessageTemplate.MatchConversationId(Board.password))
      val sender = message.getSender
      val stateOrError = decode[CTFMap](message.getContent)

      def currentData(default: (Long, CTFMap)) = data.getOrElse(sender, default)

      message.getPerformative match {
        case ACLMessage.INFORM =>
          stateOrError match {
            case Right(newData) =>
              val timed = (System.currentTimeMillis(), newData)
              val updated = data + (sender -> CTFMap.update(currentData(timed), timed))
              (updated, Response(message.getSender, None))
            case Left(err) =>
              (data, Response(sender, Some(new Error("BoardAgent: " + err.getMessage))))
          }
        case _ => (data, Response(sender, Some(new Error("BoardAgent: Performative not supported"))))
      }
  }

  def sendResponse: BehaviourFunction[BoardAgent, State, Response] = { agent =>
    (data, aa) =>

      def createMessage(perf: Int, to: AID, from: AID, content: String, password: String): ACLMessage = {
        val msg = new ACLMessage(perf)
        msg.addReceiver(to)
        msg.setSender(from)
        msg.setContent(content)
        msg.setConversationId(password)
        msg
      }

      val message = aa match {
        case Response(to, Some(err)) => createMessage(ACLMessage.FAILURE, to, getAID, err.getMessage, Board.password)
        case Response(to, None) =>
          val aggregatedData: CTFMap = data.toList.map(_._2._2).reduce((m1, m2) => m1.merge(m2))
          createMessage(ACLMessage.INFORM, to, getAID, aggregatedData.asJson.noSpaces, Board.password)
      }

      agent.send(message)
      (data, aa)
  }

  override def takeDown(): Unit = deRegister

}
