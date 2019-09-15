package com.uhu.cesar.ctf.domain

sealed trait TeamState extends Product with Serializable

object TeamState {
  case object Attacking extends TeamState
  case object WithTheFlag extends TeamState
}
