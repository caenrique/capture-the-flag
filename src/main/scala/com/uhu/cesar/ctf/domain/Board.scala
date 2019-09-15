package com.uhu.cesar.ctf.domain

import com.uhu.cesar.ctf.domain.map.CTFMap
import jade.core.AID
import jade.domain.FIPAAgentManagement.ServiceDescription

object Board {

  type State = Map[AID, (Long, CTFMap)]
  val OK = "OK"
  val emptyState = Map.empty[AID, (Long, CTFMap)]
  val password: String = "12345"
  val name: String = "BoardAgent"

  val service: ServiceDescription = {
    val sd = new ServiceDescription()
    sd.setName(name)
    sd.setType("Board")
    sd
  }

  case class Response(to: AID, content: Option[Error])
}
