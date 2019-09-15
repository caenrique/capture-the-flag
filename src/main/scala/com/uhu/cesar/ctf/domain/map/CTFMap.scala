package com.uhu.cesar.ctf.domain.map

import com.uhu.cesar.ctf.algorithms.GoTo.Point
import com.uhu.cesar.ctf.domain.AgentAction
import com.uhu.cesar.ctf.domain.AgentAction.{Adelante, Atras, Rotar}
import com.uhu.cesar.ctf.domain.CTFObject._
import com.uhu.cesar.ctf.domain.ServerMessage.ServerMessage
import monocle.macros.GenLens

case class CTFMap(walls: List[String],
                  width: Int,
                  height: Int,
                  players: Set[Player],
                  flags: Set[Flag],
                  bases: Set[Base]) {

  def isWall(p: Point): Boolean = {
    walls(p.y)(p.x) == '#'
  }

  def update(other: CTFMap): CTFMap = {
    val updated = CTFMap.walls.modify(w => CTFMap.mergeWalls(w, other.walls))
      .andThen(CTFMap.players.set(other.players))
      .andThen(CTFMap.flags.set(other.flags))
      .andThen(CTFMap.bases.set(other.bases))

    updated(other)
  }

  def merge(other: CTFMap): CTFMap = {
    val updated = CTFMap.walls.modify(w => CTFMap.mergeWalls(w, other.walls))
      .andThen(CTFMap.players.modify(p => p ++ other.players))
      .andThen(CTFMap.flags.modify(f => f ++ other.flags))
      .andThen(CTFMap.bases.modify(b => b ++ other.bases))

    updated(other)
  }

}

object CTFMap {

  val movements = List(
    (0, -1, 0), (1, -1, 45), (1, 0, 90), (1, 1, 135), (0, 1, 180), (-1, 1, 225), (-1, 0, 270), (-1, -1, 315)
  ).toSet

  def update(data1: (Long, CTFMap), data2: (Long, CTFMap)): (Long, CTFMap) = {
    if (data1._1 < data2._1) data2._1 -> data1._2.update(data2._2)
    else data2._1 -> data2._2.update(data1._2)
  }

  def mergeWalls(walls1: List[String], other: List[String]): List[String] = {
    walls1.zip(other).map{ case (a, b) => a.zip(b).map { case (cha, chb) => if (cha == ' ') chb else cha }.mkString }
  }

  val walls = GenLens[CTFMap](_.walls)
  val players = GenLens[CTFMap](_.players)
  val flags = GenLens[CTFMap](_.flags)
  val bases = GenLens[CTFMap](_.bases)

  val empty = CTFMap(Nil, 0, 0, Set.empty, Set.empty, Set.empty)

  def parse(mapString: String, width: String, height: String, message: ServerMessage): Option[CTFMap] = {

    val players = message.filter(_.obj == JUGADOR).flatMap(Player.parse).toSet
    val flags = message.filter(_.obj == BANDERA).flatMap(Flag.parse).toSet
    val bases = message.filter(_.obj == BASE).flatMap(Base.parse).toSet

    for {
      w <- width.toIntOption
      h <- height.toIntOption
    } yield CTFMap(mapString.grouped(w).toList, w, h, players, flags, bases)
  }

  def nextPosition(position: Point, heading: Int, action: AgentAction): (Point, Int) = {
    action match {
      case Adelante =>
        movements.find(_._3 == heading).map {
          case (dx, dy, _) => (Point(position.x + dx, position.y + dy), heading)
        }.get
      case Atras =>
        movements.find(_._3 == heading).map {
          case (dx, dy, _) => (Point(position.x - dx, position.y - dy), heading)
        }.get
      case Rotar(angulo) =>
        (position, (heading + angulo) % 360)
    }
  }

}
