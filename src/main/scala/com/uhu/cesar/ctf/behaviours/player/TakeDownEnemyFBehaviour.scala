package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.algorithms.GoTo
import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.domain.AgentAction.Nula
import com.uhu.cesar.ctf.domain.map.CTFMap.Point
import com.uhu.cesar.ctf.domain.map.Player

trait TakeDownEnemyFBehaviour extends ExploreFBehaviour {

  def takeDownEnemy: PlayerBehaviour = { agent =>
    (data, aa) =>
      val newPath = for {
        oldTarget <- data.target
        newTargetPosition <- data.map.players.find(p => Math.abs(p.x - oldTarget.x) <= 1 && Math.abs(p.y - oldTarget.y) <= 1 && p.team == oldTarget.team)
      } yield {
        val nextAction :: newPath = GoTo(data)((Point(data.me.x, data.me.y), data.me.heading), Point(newTargetPosition.x, newTargetPosition.y))
        (data.copy(actionList = newPath, target = Some(newTargetPosition)), nextAction)
      }

      newPath.getOrElse {
        findNearestEnemy(data).map { newTarget =>
          val nextAction :: rest = GoTo(data)((Point(data.me.x, data.me.y), data.me.heading), Point(newTarget.x, newTarget.y))
          (data.copy(actionList = rest, target = Some(newTarget)), nextAction)
        }.getOrElse {
          explore(agent)(data, aa)
        }
      }
  }

  private def findNearestEnemy(data: PlayerBehaviours.state): Option[Player] = {
    val me = data.me
    data.map.players
      .filterNot(p => p.x == me.x && p.y == me.y && p.heading == me.heading || p.team == me.team)
      .map(p => (p, Math.sqrt(Math.pow(me.x - p.x, 2) + Math.pow(me.y - p.y, 2))))
      .minByOption(_._2)
      .map(_._1)
  }

}
