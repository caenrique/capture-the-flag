package com.uhu.cesar.ctf.domain

object ServerMessage {

  type ServerMessage = List[ServerMessageLine]

  def parse(message: String): ServerMessage = {
    message.split("\n")
      .map(l => l.split(",").toList match { case x :: y :: heading :: Nil => s"YO,0,$x,$y,$heading" case _ => l})
      .map(l => if (l.split(",").length < 5) s"$l,0" else l) // ALGUNAS LINEAS VIENEN CON 3 ENTEROS Y OTRAS CON 4 !!!!
      .map(ServerMessageLine.parse).toList.flatten
  }

}
