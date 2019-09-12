package com.uhu.cesar.ctf.algorithms

import com.uhu.cesar.ctf.domain.AgentAction
import com.uhu.cesar.ctf.domain.AgentAction.{Adelante, Atras, Nula, Rotar}

object GoTo {

  case class Point(x: Int, y: Int)
  case class State(prev: Option[State], action: AgentAction, height: Int, value: Double, coords: Point, heading: Int)

  def apply(from: (Point, Int), to: Point): List[AgentAction] = {

    def heuristic(p: Point): Double = Math.sqrt(Math.pow(to.x-p.x, 2) + Math.pow(to.y-p.y, 2)) // Distancia euclidea

    def move(s: State, a: AgentAction, coords: Point): State = {
      State(Some(s), a, s.height + 1, heuristic(coords) + s.height + 1, coords, s.heading)
    }

    def rotate(s: State, da: Int): State = {
      State(Some(s), Rotar(da), s.height + 1, s.value + 1, s.coords, s.heading + da)
    }

    def neighbors(s: State): Set[State] = {
      val rotations = (0 until 360 by 45)
      val movements = List((1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1))

      val rotationChilds = rotations.drop(1).map(h => rotate(s, h)).toSet // Girar

      rotationChilds ++ rotations.zip(movements).toMap.get(s.heading).map{ case (dx, dy) =>
        Set(
          move(s, Adelante, Point(s.coords.x + dx, s.coords.y + dy)),
          move(s, Atras, Point(s.coords.x - dx, s.coords.y - dy))
        )
      }.getOrElse(Set.empty)
    }

    def buildPath(s: State): List[AgentAction] = {
      def innerBuild(s: State): List[AgentAction] = s.prev match {
        case Some(prevS) => s.action :: innerBuild(prevS)
        case None => Nil
      }
      innerBuild(s).reverse
    }

    @scala.annotation.tailrec
    def search(acc: Set[State], visited: Set[State], target: Point): List[AgentAction] = {
      if (acc.isEmpty) Nil
      else {
        val current = acc.minBy(_.value)
        if (current.coords == target) buildPath(current)
        else {
          val children = neighbors(current).filterNot(s => visited.exists(ss => ss.coords == s.coords && ss.value < s.value))
          search(acc ++ children, visited + current, target)
        }
      }
    }

    val point = Point(from._1.x, from._1.y)
    val start = State(None, Nula, 0, heuristic(point), point, from._2)

    search(Set(start), Set.empty[State], to)
  }

}
