package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.algorithms.GoTo
import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.domain.AgentAction.Nula
import com.uhu.cesar.ctf.domain.map.CTFMap
import com.uhu.cesar.ctf.domain.map.CTFMap.Point

trait GoToBaseFBehaviour extends ExploreFBehaviour {

  def goToBase: PlayerBehaviour = { agent =>
    (data, aa) =>
      val from = (Point(data.me.x, data.me.y), data.me.heading)
      val myBase = data.map.bases.find(_.team == data.me.team)

      myBase.map { base =>
        val nextAction :: newActions = if (data.computePath) {
          GoTo(data)(from, Point(base.x, base.y))
        } else if (data.actionList.nonEmpty) {
          data.actionList
        } else if (data.reactive) {
          GoTo(data)(from, Point(base.x, base.y))
        } else {
          Nula :: Nil
        }

        val nextPosition = CTFMap.nextPosition(Point(data.me.x, data.me.y), data.me.heading, nextAction)
        val nextData = if (data.actionList.isEmpty && data.reactive) data.copy(reactive = false, computePath = true)
        else data.copy(computePath = false)

        (nextData.copy(actionList = newActions, shouldBeHere = Some(nextPosition)), nextAction)
      }.getOrElse(explore(agent)(data, aa))

  }

}
