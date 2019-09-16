package com.uhu.cesar.ctf.domain

import com.uhu.cesar.ctf.domain.CTFObject.CTFObject
import com.uhu.cesar.ctf.domain.map.Team

case class ServerMessageLine(obj: CTFObject, team: Team, x: Int, y: Int, heading: Int)

object ServerMessageLine {

  def parse(messageLine: String): Option[ServerMessageLine] = {
    def buildLine(obj: String, team: String, x: String, y: String, heading: String): Option[ServerMessageLine] = {
      for {
        lobject <- CTFObject.valueOf(obj)
        tTeam <- team.toIntOption.flatMap(Team(_))
        xInt <- x.toIntOption
        yInt <- y.toIntOption
        hInt <- heading.toIntOption
      } yield ServerMessageLine(lobject, tTeam, xInt, yInt, hInt)
    }

    messageLine.split(",").toList match {
      case objStr :: team :: x :: y :: Nil => buildLine(objStr, team, x, y, "0")
      case objStr :: team :: x :: y :: heading :: Nil => buildLine(objStr, team, x, y, heading)
      case _ => None
    }
  }

}
