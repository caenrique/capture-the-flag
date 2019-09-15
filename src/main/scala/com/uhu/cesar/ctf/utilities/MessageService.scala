package com.uhu.cesar.ctf.utilities

import com.uhu.cesar.ctf.agents.BoardAgent
import com.uhu.cesar.ctf.domain.Board
import com.uhu.cesar.ctf.domain.map.CTFMap
import io.circe.Decoder
import jade.core.{AID, Agent}
import jade.domain.DFService
import jade.domain.FIPAAgentManagement.{DFAgentDescription, ServiceDescription}
import jade.domain.FIPANames.InteractionProtocol
import jade.lang.acl.{ACLMessage, MessageTemplate}
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

trait MessageService { agent: Agent =>

  // Send a message to someone given a ServiceDescription and get their AID back
  def sendMessage(sd: ServiceDescription, content: String, perf: Int): AID = {

    val template = new DFAgentDescription()
    template.addServices(sd)

    val dest = DFService.search(agent, template).toList.headOption.map(_.getName)
      .getOrElse(throw new Exception("No se puedo encontrar el AID del destinatario"))

    val request = new ACLMessage(perf)
    request.addReceiver(dest)
    request.setSender(getAID)
    request.setContent(content)
    request.setProtocol(InteractionProtocol.FIPA_REQUEST)
    request.setConversationId(Board.password)

    send(request)

    dest
  }

  def receiveJsonResponse[A](from: AID)(implicit ev: Decoder[A]): Either[Error, A] = {
    val message: ACLMessage = agent.blockingReceive(MessageTemplate.MatchConversationId(Board.password))
    message.getPerformative match {
      case ACLMessage.INFORM => decode[A](message.getContent).left.map(e => new Error(e.getMessage))
      case ACLMessage.FAILURE => Left(new Error(message.getContent))
    }
  }


}
