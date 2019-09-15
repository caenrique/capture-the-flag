package com.uhu.cesar.ctf.domain.map

import com.uhu.cesar.ctf.domain.CTFObject._
import com.uhu.cesar.ctf.domain.ServerMessageLine

case class Player(x: Int, y: Int, heading: Int, team: Team)

object Player {

  def rotate(from: Int, to: Int): Int = (360 - from + to) % 360

  def parse: ServerMessageLine => Option[Player] = {
    case ServerMessageLine(JUGADOR, team, x, y, heading) => Some(Player(x, y, heading, team))
    case ServerMessageLine(YO, team, x, y, heading) => Some(Player(x, y, heading, team))
    case _ => None
  }

  def parseFromString(x: String, y: String, heading: String, team: Int): Option[Player] = {
    for {
      xInt <- x.toIntOption
      yInt <- y.toIntOption
      hInt <- heading.toIntOption
      tTeam <- Team(team)
    } yield Player(xInt, yInt, hInt, tTeam)
  }
}