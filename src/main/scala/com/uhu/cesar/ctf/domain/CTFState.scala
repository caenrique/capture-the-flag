package com.uhu.cesar.ctf.domain

import com.uhu.cesar.ctf.algorithms.GoTo.Point
import com.uhu.cesar.ctf.domain.CTFObject._
import com.uhu.cesar.ctf.domain.ServerMessage.{ServerMessage, filterMessage}
import com.uhu.cesar.ctf.domain.TeamState.Attacking
import com.uhu.cesar.ctf.domain.map._
import jade.core.AID
import monocle.macros.GenLens

case class CTFState(map: CTFMap,
                    me: Player,
                    lastPosition: Player,
                    config: CTFConfig,
                    actionList: List[AgentAction],
                    computePath: Boolean,
                    teamState: TeamState,
                    board: Option[AID],
                    server: AID,
                    shouldBeHere: Option[(Point, Int)])

object CTFState {

  val map = GenLens[CTFState](_.map)
  val me = GenLens[CTFState](_.me)
  val players = map composeLens CTFMap.players
  val flags = map composeLens CTFMap.flags
  val walls = map composeLens CTFMap.walls
  val bases = map composeLens CTFMap.bases
  val teamState = GenLens[CTFState](_.teamState)
  val board = GenLens[CTFState](_.board)

  def parse(myTeam: Int, server: AID, message: String): Option[CTFState] = {
    println(message)
    val init :: lines = message.split("\n").toList
    val valuesList = init.split(",").toList

    val configOption = CTFConfig.parse(valuesList.take(2))
    val (playerOption, mapOption) = valuesList.drop(2) match {
      case width :: height :: x :: y :: heading :: _ :: mapString :: Nil =>
        val p = Player.parseFromString(x, y, heading, myTeam)
        val m = CTFMap.parse(mapString, width, height, lines.flatMap(ServerMessageLine.parse))
        (p, m)
      case width :: height :: x :: y :: heading :: _ :: xMin :: xMax :: yMin :: yMax :: mapString :: Nil =>
        val p = Player.parseFromString(x, y, heading, myTeam)
        val m = CTFMap.parsePartial(mapString, xMin, xMax, yMin, yMax, width, height, lines.flatMap(ServerMessageLine.parse))
        (p, m)
      case _ => (None, None)
    }

    for {
      config <- configOption
      me <- playerOption
      map <- mapOption
    } yield CTFState(map, me, me, config, Nil, computePath = true, Attacking, None, server, None)
  }

  implicit class CTFGameDataOps(gameData: CTFState) {

    def isFree(p: Point): Boolean = {
      val insideMap = (p: Point) => p.x < gameData.map.width - 1 && p.x > 0 && p.y < gameData.map.height - 1 && p.y > 0
      val notAWall = (p: Point) => !gameData.map.isWall(p)

      if (insideMap(p)) notAWall(p) else false
    }

    def updateMap(newMap: CTFMap): CTFState = gameData.copy(map = newMap)

    def updatePlayer(player: Player): CTFState = gameData.copy(me = player)

    def update(message: ServerMessage): CTFState = {

      // No echamos cuenta de las muertes

      val players = filterMessage(message, JUGADOR).toSet
      val flags = filterMessage(message, BANDERA).toSet
      val bases = filterMessage(message, BASE).toSet
      val me = filterMessage(message, YO).toSet

      val updatedData = CTFState.players.set(players.flatMap(Player.parse))
        .andThen(CTFState.flags.set(flags.flatMap(Flag.parse)))
        .andThen(CTFState.bases.set(bases.flatMap(Base.parse)))
        .andThen(me.flatMap(Player.parse).headOption.map(CTFState.me.set).getOrElse(identity[CTFState](_)))

      updatedData(gameData)
    }
  }

}
