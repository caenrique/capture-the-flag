package com.uhu.cesar.ctf.domain

import com.uhu.cesar.ctf.domain
import com.uhu.cesar.ctf.domain.CTFObject.CTFObject
import com.uhu.cesar.ctf.domain.map.Team
import com.uhu.cesar.ctf.domain.map.Team.Team

case class ServerMessageLine(obj: CTFObject, team: Team, x: Int, y: Int, heading: Int)

object ServerMessageLine {

  def parse(messageLine: String): Option[ServerMessageLine] = {
    def buildLine(obj: String, team: String, x: String, y: String, heading: String): Option[ServerMessageLine] = {
      CTFObject.valueOf(obj).map(o => ServerMessageLine(o, Team(team.toInt), x.toInt, y.toInt, heading.toInt))
    }

    messageLine.split(",").toList match {
      case objStr :: team :: x :: y :: Nil => buildLine(objStr, team, x, y, "0")
      case objStr :: team :: x :: y :: heading :: Nil => buildLine(objStr, team, x, y, heading)
    }
  }

}
