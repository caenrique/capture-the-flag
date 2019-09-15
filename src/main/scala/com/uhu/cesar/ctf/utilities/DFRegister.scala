package com.uhu.cesar.ctf.utilities

import jade.core.Agent
import jade.domain.DFService
import jade.domain.FIPAAgentManagement.{DFAgentDescription, ServiceDescription}

import scala.util.Try

trait DFRegister { agent: Agent =>

  def registerService(serviceDescription: ServiceDescription): Try[DFAgentDescription] = {
    val dfDescription = new DFAgentDescription
    dfDescription.setName(agent.getAID)

    dfDescription.addServices(serviceDescription)

    Try(DFService.register(agent, dfDescription))
  }

  def deRegister: Try[Unit] = Try(DFService.deregister(agent))

}
