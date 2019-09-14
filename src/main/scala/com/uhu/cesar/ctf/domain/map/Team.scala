package com.uhu.cesar.ctf.domain.map

sealed trait Team extends Product with Serializable

object Team extends {
  case object RED extends Team
  case object BLUE extends Team

  def apply(id: Int): Option[Team] = id match {
    case 0 => Some(RED)
    case 1 => Some(BLUE)
    case _ => None
  }
}
