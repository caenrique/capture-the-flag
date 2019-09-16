package com.uhu.cesar.ctf.domain.map

import com.uhu.cesar.ctf.domain.map.Team.{BLUE, RED}

sealed trait Team extends Product with Serializable {
  val id: Int
}

object Team extends {
  case object RED extends Team { val id = 0 }
  case object BLUE extends Team { val id = 1 }

  def apply(id: Int): Option[Team] = id match {
    case 0 => Some(RED)
    case 1 => Some(BLUE)
    case _ => None
  }
}
