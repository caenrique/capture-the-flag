package com.uhu.cesar.ctf.domain

import com.uhu.cesar.ctf.algorithms.GoTo.Point
import com.uhu.cesar.ctf.domain
import com.uhu.cesar.ctf.domain.CTFObject._
import com.uhu.cesar.ctf.domain.ServerMessage.ServerMessage
import com.uhu.cesar.ctf.domain.map.{Base, CTFMap, Flag, Player, Team}
import com.uhu.cesar.ctf.utilities.OptionUtils._
import jade.lang.acl.ACLMessage

// TODO: Ordenar para separar inner-state, outer-state y config
// TODO: cambiar nombre a uno más significativo. Ej: CTFState
case class CTFGameData(
                        map: CTFMap,
                        players: List[Player],
                        me: Player,
                        lastPosition: Player,
                        redFlag: Flag,
                        blueFlag: Flag,
                        redBase: Base,
                        blueBase: Base,
                        config: CTFConfig,
                        lastMessage: Option[ACLMessage],
                        actionList: List[AgentAction],
                        computePath: Boolean
                      )

object CTFGameData {

  def parse(myTeam: Int, message: String): Option[CTFGameData] = {
    val init :: lines = message.split("\n").toList
    val withOrientation :: withPartialVision :: rest = init.split(",").toList
    val width :: height :: x :: y :: heading :: _ :: mapString :: Nil = rest

    val config = CTFConfig(withPartialVision.toBoolean, withOrientation.toBoolean)
    val map = CTFMap(mapString.grouped(width.toInt).toList, height.toInt, width.toInt)

    val defaultData = domain.CTFGameData(
      map,
      Nil,
      Player(x.toInt, y.toInt, heading.toInt, Team(myTeam)),
      Player(x.toInt, y.toInt, heading.toInt, Team(myTeam)),
      Flag(0, 0, Team.RED), // To be updated
      Flag(0, 0, Team.BLUE), // To be updated
      Base(0, 0), // to be updated
      Base(0, 0), // to be updated
      config,
      None,
      Nil,
      true
    )

    lines.map(ServerMessageLine.parse).sequence.map { message =>
      println(s"message: $message")
      defaultData.update(message)
    }

  }

  implicit class CTFGameDataOps(gameData: CTFGameData) {

    def isFree(p: Point): Boolean = {
      val insideMap = (p: Point) => p.x < gameData.map.width - 1 && p.x > 0 && p.y < gameData.map.height - 1 && p.y > 0
      val notAWall = (p: Point) => !gameData.map.isWall(p)

      if(insideMap(p)) notAWall(p) else false
    }

    def update(message: ServerMessage): CTFGameData = {

      val (players, otherlines) = message.partition { l => l.obj == JUGADOR }
      val (me, rest) = otherlines.partition{ l => l.obj == YO }
      val withPlayers = gameData.copy(players = players.flatMap(Player.parse))
      val withMe = if (me.nonEmpty) {
        withPlayers.copy(me = me.flatMap(Player.parse).head.copy(team = gameData.me.team))
      } else withPlayers

      rest.foldLeft(withMe) { (data: CTFGameData, m: ServerMessageLine) =>
        m.obj match {
          case MUERTE =>
            val withOutDead = data.players.filterNot(p => p.x == m.x && p.y == m.y && p.team == m.team)
            data.copy(players = withOutDead)
          case BANDERA =>
            if (m.team == Team.RED) data.copy(redFlag = Flag(m.x, m.y, Team.RED))
            else data.copy(blueFlag = Flag(m.x, m.y, Team.BLUE))
          case BASE =>
            if (m.team == Team.RED) data.copy(redBase = Base(m.x, m.y))
            else data.copy(blueBase = Base(m.x, m.y))
          case YO =>
            data.copy(me = data.me.copy(x = m.x, y = m.y, heading = m.heading))
          case _ => data
        }
      }
    }
  }

}
