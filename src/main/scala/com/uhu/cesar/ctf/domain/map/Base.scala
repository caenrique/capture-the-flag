package com.uhu.cesar.ctf.domain.map

import com.uhu.cesar.ctf.domain.CTFObject.BASE
import com.uhu.cesar.ctf.domain.ServerMessageLine

case class Base(x: Int, y: Int, team: Team)

object Base {
  def parse: ServerMessageLine => Option[Base] = {
    case ServerMessageLine(BASE, team, x, y, _) => Some(Base(x, y, team))
    case _ => None
  }
}
