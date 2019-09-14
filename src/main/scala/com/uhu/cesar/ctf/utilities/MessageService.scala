package com.uhu.cesar.ctf.utilities

import jade.core.{AID, Agent}
import jade.domain.DFService
import jade.domain.FIPAAgentManagement.{DFAgentDescription, ServiceDescription}
import jade.domain.FIPANames.InteractionProtocol
import jade.lang.acl.ACLMessage

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

    send(request)

    dest
  }


}
