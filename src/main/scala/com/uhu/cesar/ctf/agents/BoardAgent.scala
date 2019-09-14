package com.uhu.cesar.ctf.agents

import jade.domain.FIPAAgentManagement.ServiceDescription

class BoardAgent {

}

object BoardAgent {
  val name: String = "BoardAgent"
  val service: ServiceDescription = {
    val sd = new ServiceDescription()
    sd.setName(name)
    sd.setType("Board")
    sd
  }
}
