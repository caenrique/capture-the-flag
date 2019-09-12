package com.uhu.cesar.ctf.behaviours

import jade.core.Agent

abstract class FBehaviour[A <: Agent, S, AA](agent: A) {
  def action(state: S, prevAction: AA): (S, AA)
  def done(state: S): Boolean
}
