package com.uhu.cesar.ctf.behaviours

import jade.core.Agent
import jade.core.behaviours.Behaviour

class LoopBehaviour[A <: Agent, S, AA](agent: A,
                                       initialData: S,
                                       initialAction: AA,
                                       behaviours: List[FBehaviour[A, S, AA]]
                                      ) extends Behaviour(agent) {

  var state: S = _
  var agentAction: AA = _

  override def onStart(): Unit = {
    state = initialData
    agentAction = initialAction
  }

  override def action() = {
    val (newState, newAgentAction) = behaviours.foldLeft((state, agentAction)) {
      case ((data, a), b) => b.action(data, a)
    }
    state = newState
    agentAction = newAgentAction
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
