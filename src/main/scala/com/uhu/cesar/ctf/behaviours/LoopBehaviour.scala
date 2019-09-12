package com.uhu.cesar.ctf.behaviours

import jade.core.Agent
import jade.core.behaviours.Behaviour

class LoopBehaviour[A <: Agent, S, AA](agent: A,
                                       initialData: S,
                                       initialAction: AA,
                                       behaviours: List[FBehaviour[A, S, AA]]
                                      ) extends Behaviour(agent) {

  override def action() = {
    behaviours.foldLeft((initialData, initialAction)) { case ((data, a), b) => b.action(data, a) }
  }

  override def done(): Boolean = false

}


object LoopBehaviour {

  type BehaviourFunction[A <: Agent, S, AA] = A => (S, AA) => (S, AA)

  def createChildBehaviour[A <: Agent, S, AA](agent: A)(actionf: BehaviourFunction[A, S, AA]): FBehaviour[A, S, AA] = {
    new FBehaviour[A, S, AA](agent) {
      override def action(state: S, action: AA): (S, AA) = actionf(agent)(state, action)

      override def done(state: S): Boolean = true
    }
  }
}
