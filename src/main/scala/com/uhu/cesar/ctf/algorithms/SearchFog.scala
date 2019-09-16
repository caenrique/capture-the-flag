package com.uhu.cesar.ctf.algorithms

import com.uhu.cesar.ctf.domain.AgentAction.{Adelante, Atras, Nula, Rotar}
import com.uhu.cesar.ctf.domain.map.CTFMap.Point
import com.uhu.cesar.ctf.domain.map.{CTFMap, Player}
import com.uhu.cesar.ctf.domain.{AgentAction, CTFState}

object SearchFog {

  case class State(prev: Option[State], actions: List[AgentAction], height: Int, value: Double, coords: Point, heading: Int)

  def apply(data: CTFState): List[AgentAction] = {

    val me = data.me

    def heuristic(p: Point): Double = Math.sqrt(Math.pow(me.x - p.x, 2) + Math.pow(me.y - p.y, 2)) // Distancia euclidea

    def neighbors(s: State): Set[State] = {

      def move(s: State, a: AgentAction, coords: Point): State = {
        State(Some(s), List(a), s.height + 1, heuristic(coords) + s.height + 1, coords, s.heading)
      }

      def rotate(s: State, da: Int, coords: Point): State = {
        State(Some(s), List(Rotar(da), Adelante), s.height + 2, heuristic(coords) + s.height + 2, coords, (s.heading + da) % 360)
      }

      CTFMap.movements
        .map { case (dx, dy, da) => (Point(s.coords.x + dx, s.coords.y + dy), Player.rotate(s.heading, da)) }
        .filter { case (p, _) => !data.map.isWall(p) }
        .map {
          case (p, 0) => move(s, Adelante, p)
          case (p, 180) => move(s, Atras, p)
          case (p, da) => rotate(s, da, p)
        }
    }

    def buildPath(s: State): List[AgentAction] = {
      @scala.annotation.tailrec
      def innerBuild(s: State, actions: List[AgentAction]): List[AgentAction] = s.prev match {
        case Some(prevS) => innerBuild(prevS, s.actions ++ actions)
        case None => actions
      }

      innerBuild(s, Nil)
    }

    @scala.annotation.tailrec
    def search(acc: Set[State], visited: Set[State]): List[AgentAction] = {
      if (acc.isEmpty) Nil
      else {
        val current = acc.minBy(_.value)
        if (data.map.walls(current.coords.y)(current.coords.x) == CTFMap.FOG) buildPath(current)
        else {
          val inVisited = (s: State) => visited.exists(ss => ss.coords == s.coords && ss.value <= s.value)
          val inAcc = (s: State) => acc.exists(ss => ss.coords == s.coords && ss.value <= s.value)
          val children = neighbors(current).filterNot(s => inVisited(s) || inAcc(s))
          search((acc - current) ++ children, visited + current)
        }
      }
    }

    val point = Point(me.x, me.y)
    val start = State(None, Nil, 0, heuristic(point), point, me.heading)

    val path = search(Set(start), Set.empty[State])

    if (path.isEmpty) Nula :: Nil else path
  }

}

