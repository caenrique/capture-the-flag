package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.algorithms.GoTo.Point
import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.domain.AgentAction
import com.uhu.cesar.ctf.domain.AgentAction.{Adelante, Rotar}
import com.uhu.cesar.ctf.domain.map.{CTFMap, Player}

import scala.util.Random

trait ExploreFBehaviour {

  def explore: PlayerBehaviour = { agent =>
    (data, aa) =>
      def isFree(mov: (Int, Int, Int)): Boolean = {
        data.isFree(Point(data.me.x + mov._1, data.me.y + mov._2))
      }

      def toAction(mov: (Int, Int, Int)): AgentAction = {
        Rotar(Player.rotate(data.me.heading, mov._3))
      }

      val nextAction = CTFMap.movements.find(_._3 == data.me.heading).map { case (dx, dy, _) =>
        if (data.isFree(Point(data.me.x + dx, data.me.y + dy))) Adelante
        else {
          println("Nueva direccion aleatoria")
          Random.shuffle(CTFMap.movements.filter(isFree).map(toAction)).headOption.getOrElse(aa)
        }
      }.getOrElse(aa)

      (data, nextAction)
  }

}
