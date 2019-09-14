package com.uhu.cesar.ctf.utilities

import com.uhu.cesar.ctf.agents.PlayerAgent
import jade.core.AID
import jade.domain.DFService
import jade.domain.FIPAAgentManagement.{DFAgentDescription, ServiceDescription}
import jade.domain.FIPANames.InteractionProtocol
import jade.lang.acl.ACLMessage
import jade.proto.SimpleAchieveREInitiator

trait ServerConnectionService { agent: PlayerAgent =>

  def connectToServerMessage(team: Int, password: String, teamName: String): ACLMessage = {
    val request = new ACLMessage(ACLMessage.REQUEST)
    request.addReceiver(serverAID)
    request.setSender(getAID)
    request.setContent(s"$team,$password,$teamName")
    request.setProtocol(InteractionProtocol.FIPA_REQUEST)

    request
  }

  def getConnectionBehaviour(message: ACLMessage, informHandler: (String, ACLMessage) => Unit): SimpleAchieveREInitiator = {
    new SimpleAchieveREInitiator(agent, message) {
      override def handleInform(msg: ACLMessage): Unit = informHandler("inform", msg)

      override def handleRefuse(msg: ACLMessage): Unit = informHandler("refuse", msg)

      override def handleFailure(msg: ACLMessage): Unit = informHandler("failure", msg)

    }
  }

  def serverAID: AID = {
    val sd = new ServiceDescription()
    sd.setName("SERVIDOR_2019")
    sd.setType("CTF")

    val template = new DFAgentDescription()
    template.addServices(sd)

    DFService.search(agent, template).toList.headOption.map(_.getName)
      .getOrElse(throw new Exception("No se puedo encontrar el AID del servidor"))
  }

}
