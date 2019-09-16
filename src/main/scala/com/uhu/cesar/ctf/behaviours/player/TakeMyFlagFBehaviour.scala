package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.algorithms.GoTo
import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.domain.AgentAction.Nula
import com.uhu.cesar.ctf.domain.map.CTFMap
import com.uhu.cesar.ctf.domain.map.CTFMap.Point

trait TakeMyFlagFBehaviour {

  def takeMyFlag: PlayerBehaviour = { agent =>
    (data, aa) =>

      val from = (Point(data.me.x, data.me.y), data.me.heading)
      val myFlagOption = data.map.flags.find(_.team == data.me.team)

      if (myFlagOption.isDefined) { // TODO: reestructura esto para hacerlo con un map

        val myFlag = myFlagOption.get
        val nextAction :: newActions = if (data.computePath) {
          GoTo(data)(from, Point(myFlag.x, myFlag.y))
        } else if (data.actionList.nonEmpty) {
          data.actionList
        } else if (data.reactive) {
          GoTo(data.copy(reactive = false, computePath = true))(from, Point(myFlag.x, myFlag.y))
        } else {
          Nula :: Nil
        }

        val nextPosition = CTFMap.nextPosition(Point(data.me.x, data.me.y), data.me.heading, nextAction)
        val nextData = if (data.actionList.isEmpty && data.reactive) data.copy(reactive = false, computePath = true)
        else data.copy(computePath = false)

        (nextData.copy(actionList = newActions, shouldBeHere = Some(nextPosition)), nextAction)
      } else (data, aa) // Si no ve la bandera, no hagas nada
  }
}
