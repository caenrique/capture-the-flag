package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.algorithms.SearchFog
import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.domain.map.CTFMap
import com.uhu.cesar.ctf.domain.map.CTFMap.Point

trait ExploreFBehaviour {

  def explore: PlayerBehaviour = { agent =>
    (data, aa) =>
      if (data.computePath || data.actionList.isEmpty) {
        val nextAction :: rest = SearchFog(data)
        val nextPosition = CTFMap.nextPosition(Point(data.me.x, data.me.y), data.me.heading, nextAction)
        (data.copy(actionList = rest, computePath = false, shouldBeHere = Some(nextPosition), reactive = false), nextAction)
      } else {
        val nextAction :: rest = data.actionList
        val nextPosition = CTFMap.nextPosition(Point(data.me.x, data.me.y), data.me.heading, nextAction)
        (data.copy(actionList = rest, shouldBeHere = Some(nextPosition)), nextAction)
      }
  }

}
