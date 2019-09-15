package com.uhu.cesar.ctf.behaviours.player

import com.uhu.cesar.ctf.agents.PlayerAgent
import com.uhu.cesar.ctf.behaviours.LoopBehaviour.BehaviourFunction
import com.uhu.cesar.ctf.domain.{AgentAction, CTFState}

trait PlayerBehaviours extends ReceiveMessagesFBehaviour
  with TalkToBoardFBehaviour
  with ListenToTeamFBehaviour
  with TakeTheFlagFBehaviour
  with GoToBaseFBehaviour
  with TakeDownEnemyFBehaviour
  with ExploreFBehaviour
  with SendActionFBehaviour
  with ChangeTeamStateFBehaviour
  with CheckPositionFBehaviour

object PlayerBehaviours {
  type PlayerBehaviour = BehaviourFunction[PlayerAgent, CTFState, AgentAction]
}
