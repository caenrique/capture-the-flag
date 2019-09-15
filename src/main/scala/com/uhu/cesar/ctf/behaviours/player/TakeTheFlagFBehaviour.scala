package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.algorithms.GoTo
import com.uhu.cesar.ctf.algorithms.GoTo.Point
import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.domain.AgentAction.Nula
import com.uhu.cesar.ctf.domain.map.CTFMap

trait TakeTheFlagFBehaviour {

  def takeTheFlag: PlayerBehaviour = { agent =>
    (data, aa) =>

      val from = (Point(data.me.x, data.me.y), data.me.heading)
      val enemyFlagOption = data.map.flags.find(_.team != data.me.team)

      if (enemyFlagOption.isDefined) { // TODO: reestructura esto para hacerlo con un map

        val enemyFlag = enemyFlagOption.get
        val nextAction :: newActions = if (data.computePath) {
          GoTo(data)(from, Point(enemyFlag.x, enemyFlag.y))
        } else if (data.actionList.nonEmpty) {
          data.actionList
        } else {
          Nula :: Nil
        }

        val nextPosition = CTFMap.nextPosition(Point(data.me.x, data.me.y), data.me.heading, nextAction)

        (data.copy(computePath = false, actionList = newActions, shouldBeHere = Some(nextPosition)), nextAction)
      } else (data, aa) // Si no ve la bandera, no hagas nada
  }
}
