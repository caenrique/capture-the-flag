package com.uhu.cesar.ctf.domain.map

import com.uhu.cesar.ctf.algorithms.GoTo.Point
import com.uhu.cesar.ctf.domain.CTFObject._
import com.uhu.cesar.ctf.domain.ServerMessage.ServerMessage
import monocle.macros.GenLens

case class CTFMap(walls: List[String],
                  width: Int,
                  height: Int,
                  players: List[Player],
                  flags: List[Flag],
                  bases: List[Base]) {

  def isWall(p: Point): Boolean = {
    walls(p.y)(p.x) == '#'
  }
}

object CTFMap {

  val walls = GenLens[CTFMap](_.walls)
  val players = GenLens[CTFMap](_.players)
  val flags = GenLens[CTFMap](_.flags)
  val bases = GenLens[CTFMap](_.bases)


  def parse(mapString: String, width: String, height: String, message: ServerMessage): Option[CTFMap] = {

    val players = message.filter(_.obj == JUGADOR).flatMap(Player.parse)
    val flags = message.filter(_.obj == BANDERA).flatMap(Flag.parse)
    val bases = message.filter(_.obj == BASE).flatMap(Base.parse)

    for {
      w <- width.toIntOption
      h <- height.toIntOption
    } yield CTFMap(mapString.grouped(w).toList, w, h, players, flags, bases)
  }
}
