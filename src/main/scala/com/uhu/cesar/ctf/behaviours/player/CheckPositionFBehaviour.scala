package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.behaviours.player.PlayerBehaviours.PlayerBehaviour
import com.uhu.cesar.ctf.domain.map.CTFMap.Point

trait CheckPositionFBehaviour {

  def checkPosition: PlayerBehaviour = { agent =>
    (data, aa) =>
      data.shouldBeHere.map { case (Point(x, y), heading) =>
        if (data.me.x == x && data.me.y == y && data.me.heading == heading) (data, aa)
        else (data.copy(computePath = true), aa)
      }.getOrElse((data, aa))
  }

}
