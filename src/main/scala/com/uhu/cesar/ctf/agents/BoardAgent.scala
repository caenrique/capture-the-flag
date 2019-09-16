package com.uhu.cesar.ctf.agents

import com.uhu.cesar.ctf.behaviours.LoopBehaviour
import com.uhu.cesar.ctf.behaviours.LoopBehaviour.BehaviourFunction
import com.uhu.cesar.ctf.domain.Board.{BroadcastResponse, Response, SingleResponse, State}
import com.uhu.cesar.ctf.domain.map.CTFMap
import com.uhu.cesar.ctf.domain.{Board, TeamState}
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
        SingleResponse(getAID, None),
        behaviours.map(LoopBehaviour.createChildBehaviour[BoardAgent, State, Response](this))
      )
    )

  }

  def receiveMessage: BehaviourFunction[BoardAgent, State, Response] = { agent =>
    (data, _) =>

      val template = MessageTemplate.or(
        MessageTemplate.MatchConversationId(Board.password),
        MessageTemplate.MatchConversationId(Board.teamComunication)
      )
      val message: ACLMessage = agent.blockingReceive(template)
      val sender = message.getSender

      def currentData(default: (Long, CTFMap)) = data.getOrElse(sender, default)

      message.getPerformative match {
        case ACLMessage.INFORM =>
          decode[CTFMap](message.getContent) match {
            case Right(newData) =>
              val timed = (System.currentTimeMillis(), newData)
              val updated = data + (sender -> CTFMap.update(currentData(timed), timed))
              (updated, SingleResponse(message.getSender, None))
            case Left(err) =>
              (data, SingleResponse(sender, Some(new Error("BoardAgent: " + err.getMessage))))
          }
        case ACLMessage.PROPAGATE =>
          decode[TeamState](message.getContent) match {
            case Right(newState) =>
              (data, BroadcastResponse(newState))
            case Left(err) =>
              (data, SingleResponse(sender, Some(new Error("BoardAgent: " + err.getMessage))))
          }
        case _ => (data, SingleResponse(sender, Some(new Error("BoardAgent: Performative not supported"))))
      }
  }

  def sendResponse: BehaviourFunction[BoardAgent, State, Response] = { agent =>
    (data, aa) =>

      def createMessage(perf: Int, to: List[AID], from: AID, content: String, password: String): ACLMessage = {
        val msg = new ACLMessage(perf)
        to.foreach(msg.addReceiver)
        msg.setSender(from)
        msg.setContent(content)
        msg.setConversationId(password)
        msg
      }

      val message = aa match {
        case SingleResponse(to, Some(err)) => createMessage(ACLMessage.FAILURE, List(to), getAID, err.getMessage, Board.password)
        case SingleResponse(to, None) =>
          val aggregatedData: CTFMap = data.toList.map(_._2._2).reduce((m1, m2) => m1.merge(m2))
          println("aggregated: " + aggregatedData)
          createMessage(ACLMessage.INFORM, List(to), getAID, aggregatedData.asJson.noSpaces, Board.password)
        case BroadcastResponse(newState) =>
          createMessage(ACLMessage.INFORM, data.keys.toList, getAID, newState.asJson.noSpaces, Board.teamComunication)
      }

      agent.send(message)
      (data, aa)
  }

  override def takeDown(): Unit = deRegister

}
