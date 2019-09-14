package com.uhu.cesar.ctf.domain.map

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util.Using

class CTFStateSpec extends FlatSpec with Matchers {

  def loadMap(filename: String): Option[CTFMap] = {
    Using(Source.fromFile(filename, "UTF-8")) {
      reader => reader.getLines().toList
    }.map(ls => CTFMap(ls, ls.size, ls.head.length, Nil, Nil, Nil)).toOption
  }

  "A group of players" should "be trackeable where they came from" in {

    // TODO: Hacer test en condiciones
    val enemies = Map(
      0 -> Player(1, 1, 90, Team.RED),
      0 -> Player(5, 5, 0, Team.RED),
    )
  }

}
