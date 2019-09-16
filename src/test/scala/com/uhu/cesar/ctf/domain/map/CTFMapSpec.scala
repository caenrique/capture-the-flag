package com.uhu.cesar.ctf.domain.map

import com.uhu.cesar.ctf.algorithms.GoTo.Point
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util.Using

class CTFMapSpec extends FlatSpec with Matchers {

  def loadMap(filename: String): Option[CTFMap] = {
    Using(Source.fromFile(filename, "UTF-8")) {
      reader => reader.getLines().toList
    }.map(ls => CTFMap(ls, ls.size, ls.head.length, Set.empty, Set.empty, Set.empty)).toOption
  }

  "a partial Map" should "have the same width and height when expanded than a full map" in {

    val width = 5
    val height = 10
    val partialMap = "####  #  "
    val upperLeft = Point(1, 1)
    val lowerRight = Point(3, 3)

    val expanded = CTFMap.expand(partialMap, upperLeft, lowerRight, width, height)

    println(partialMap.grouped(3).mkString("|\n"))
    println(expanded.mkString("|\n"))

    expanded.length shouldEqual height
    expanded(0).length shouldEqual width

  }

  "a map" should "maintain the observed parts instead of fog" in {
    val walls1 = List("-----","-   -","-   -","-   -","-----")
    val walls2 = List("###--","#  --","#  --","-----","-----")
    val walls3 = List("###--","#   -","#   -","-   -","-----")
    val merged = CTFMap.mergeWalls(walls1, walls2)

    merged shouldEqual walls3
  }

}
