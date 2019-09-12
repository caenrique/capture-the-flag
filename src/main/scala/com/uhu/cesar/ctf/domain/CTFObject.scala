package com.uhu.cesar.ctf.domain

object CTFObject extends Enumeration {

  type CTFObject = Value
  val ENTRADA, JUGADOR, MUERTE, BASE, BANDERA, YO = Value

  def valueOf(name: String) = values.find(_.toString == name.toUpperCase())
}
