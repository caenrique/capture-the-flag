package com.uhu.cesar.ctf.domain.map

import com.uhu.cesar.ctf.domain.ServerMessageLine
import com.uhu.cesar.ctf.domain.map.Team.Team
import com.uhu.cesar.ctf.domain.CTFObject._

case class Player(x: Int, y: Int, heading: Int, team: Team)

object Player {
  def parse(line: ServerMessageLine): Option[Player] = {
    line.obj match {
      case JUGADOR => Some(Player(line.x, line.y, line.heading, line.team))
      case YO => Some(Player(line.x, line.y, line.heading, line.team))
      case _ => None
    }
  }
}