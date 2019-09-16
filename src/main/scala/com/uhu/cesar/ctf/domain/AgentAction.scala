package com.uhu.cesar.ctf.domain

import scala.util.Random

sealed trait AgentAction extends Product with Serializable

object AgentAction {

  case object Nula extends AgentAction
  case object Abandonar extends AgentAction
  case object Adelante extends AgentAction
  case object Atras extends AgentAction
  case class Rotar(angulo: Int) extends AgentAction {
    require(angulo % 45 == 0, "El ángulo de la rotación tiene que ser múltiplo de 45")
    override def toString: String = s"Rotar $angulo"
  }
}
