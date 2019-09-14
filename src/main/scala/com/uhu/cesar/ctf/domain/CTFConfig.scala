package com.uhu.cesar.ctf.domain

case class CTFConfig(usingHeading: Boolean, partialVision: Boolean)

object CTFConfig {
  def parse: List[String] => Option[CTFConfig] = {
    case orientation :: partialVision :: Nil =>
      for {
        or <- orientation.toBooleanOption
        pv <- partialVision.toBooleanOption
      } yield CTFConfig(or, pv)
    case _ => None
  }
}
