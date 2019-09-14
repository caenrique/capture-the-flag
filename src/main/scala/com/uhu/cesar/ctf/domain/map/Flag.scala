package com.uhu.cesar.ctf.domain.map

import com.uhu.cesar.ctf.domain.CTFObject.BANDERA
import com.uhu.cesar.ctf.domain.ServerMessageLine

case class Flag(x: Int, y: Int, team: Team)

object Flag {
  def parse: ServerMessageLine => Option[Flag] = {
    case ServerMessageLine(BANDERA, team, x, y, _) => Some(Flag(x, y, team))
    case _ => None
  }
}
