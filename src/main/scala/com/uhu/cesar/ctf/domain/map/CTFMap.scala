package com.uhu.cesar.ctf.domain.map

import com.uhu.cesar.ctf.algorithms.GoTo.Point

case class CTFMap(walls: List[String], height: Int, width: Int) {
  def isWall(p: Point): Boolean = {
    walls(p.y)(p.x) == '#'
  }
}
