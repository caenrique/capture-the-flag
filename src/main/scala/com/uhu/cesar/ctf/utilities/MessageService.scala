package com.uhu.cesar.ctf.utilities

import com.uhu.cesar.ctf.domain.Board
import io.circe.Decoder
import io.circe.parser._
import jade.core.{AID, Agent}
import jade.domain.DFService
import jade.domain.FIPAAgentManagement.{DFAgentDescription, ServiceDescription}
import jade.domain.FIPANames.InteractionProtocol
import jade.lang.acl.{ACLMessage, MessageTemplate}

trait MessageService {

  def sendMessage(agent: Agent, to: AID, content: String, perf: Int): Unit = {
    val request = new ACLMessage(perf)
    request.addReceiver(to)
    request.setSender(agent.getAID)
    request.setContent(content)
    request.setProtocol(InteractionProtocol.FIPA_REQUEST)
    request.setConversationId(Board.password)

    agent.send(request)
  }

  // Send a message to someone given a ServiceDescription and get their AID back
  def sendMessage(agent: Agent, sd: ServiceDescription, content: String, perf: Int): AID = {

    val template = new DFAgentDescription()
    template.addServices(sd)

    val dest = DFService.search(agent, template).toList.headOption.map(_.getName)
      .getOrElse(throw new Exception("No se puedo encontrar el AID del destinatario"))

    sendMessage(agent, dest, content, perf)
    dest
  }

  def receiveJsonResponse[A](agent: Agent, from: AID)(implicit ev: Decoder[A]): Either[Error, A] = {
    val message: ACLMessage = agent.blockingReceive(MessageTemplate.MatchConversationId(Board.password))
    message.getPerformative match {
      case ACLMessage.INFORM => decode[A](message.getContent).left.map(e => new Error(e.getMessage))
      case ACLMessage.FAILURE => Left(new Error(message.getContent))
    }
  }


}
