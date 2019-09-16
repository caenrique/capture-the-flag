package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.agents.PlayerAgent
import com.uhu.cesar.ctf.behaviours.LoopBehaviour.BehaviourFunction
import com.uhu.cesar.ctf.domain.{AgentAction, CTFState}

trait PlayerBehaviours extends ReceiveMessagesFBehaviour
  with TalkToBoardFBehaviour
  with ListenToTeamFBehaviour
  with TakeTheFlagFBehaviour
  with TakeMyFlagFBehaviour
  with GoToBaseFBehaviour
  with TakeDownEnemyFBehaviour
  with ExploreFBehaviour
  with SendActionFBehaviour
  with ChangeTeamStateFBehaviour
  with CheckPositionFBehaviour
  with DodgePlayerFBehaviour

object PlayerBehaviours {
  type agent = PlayerAgent
  type state = CTFState
  type action = AgentAction
  type PlayerBehaviour = BehaviourFunction[agent, state, action]
}
