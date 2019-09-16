package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.domain.AgentAction
import com.uhu.cesar.ctf.domain.AgentAction.{Adelante, Rotar}
import com.uhu.cesar.ctf.domain.TeamState.{Attacking, WithTheFlag}
import com.uhu.cesar.ctf.domain.map.CTFMap
import com.uhu.cesar.ctf.domain.map.CTFMap.Point

trait DodgePlayerFBehaviour {

  def dodgePlayer: PlayerBehaviour = { agent =>
    (data, aa) =>
      val meWithFlag = data.map.flags.exists(f => f.x == data.me.x && f.y == data.me.y)
      data.teamState match {
        case Attacking => dodge(agent)(data, aa)
        case WithTheFlag =>
          if (meWithFlag) {
            println("dodge with flag")
            dodge(agent)(data, aa)
          } else (data, aa)
      }
  }

  private def dodge: PlayerBehaviour = { agent =>
    (data, aa) =>
      val me = data.me
      CTFMap.movements.find(_._3 == me.heading % 360).flatMap { case (dx, dy, heading) =>
        aa match {
          case AgentAction.Adelante =>
            val sidePosition = (angle: Int) => CTFMap.movements.find(_._3 == (heading + angle) % 360)
              .filter { case (dx, dy, _) => data.isFree(Point(me.x + dx, me.y + dy)) }
              .map(_ => angle)

            val freePosition = sidePosition(45).orElse(sidePosition(-45))
            val playerInFront = data.map.players.find(p => p.x == me.x + dx*2 && p.y == me.y + dy*2)

            if (playerInFront.isDefined) println(s"Player in front")

            for {
              toAngle <- freePosition
              _ <- playerInFront
            } yield {
              val next :: rest = List(Rotar(toAngle), Adelante, Rotar(-(2*toAngle)), Adelante, Rotar(toAngle))
              val nextPosition = CTFMap.nextPosition(Point(data.me.x, data.me.y), data.me.heading, next)
              (data.copy(actionList = rest, reactive = true, shouldBeHere = Some(nextPosition)), next)
            }
          case AgentAction.Atras =>
            val sidePosition = (angle: Int) => CTFMap.movements.find(_._3 == (heading + angle) % 360)
              .filter { case (dx, dy, _) => data.isFree(Point(me.x + dx, me.y + dy)) }
              .map(_ => angle)

            val freePosition = sidePosition(135).orElse(sidePosition(-135))
            val playerBack = data.map.players.find(p => p.x == me.x - dx*2 && p.y == me.y - dy*2)

            for {
              toAngle <- freePosition
              _ <- playerBack
            } yield {
              val next :: rest = List(Rotar(toAngle), Adelante, Rotar(-(2*toAngle)), Adelante, Rotar(toAngle))
              val nextPosition = CTFMap.nextPosition(Point(data.me.x, data.me.y), data.me.heading, next)
              (data.copy(actionList = rest, reactive = true, shouldBeHere = Some(nextPosition)), next)
            }
          case _ => None
        }
      }.getOrElse(data -> aa)
  }

}
