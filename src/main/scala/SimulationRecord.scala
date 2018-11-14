import Networks.Agent

// An action, represented by its ID
case class Action (id: Int) extends AnyVal {
  override def toString: String = s"Act$id"
}

// A record of the interaction between two agents, giving their actions and the instigator's reward
case class InteractionRecord (round: Int, instigatorAgent: Agent, receiverAgent: Agent,
                              instigatorAction: Action, receiverAction: Action,
                              instigatorReward: Double, receiverReward: Double) {
  override def toString =
    s"$round: [$instigatorAgent:$instigatorAction]($instigatorReward) <=> [$receiverAgent:$receiverAction]($receiverReward)"
}

// The log of results of the simulation, aggregated as we go along to conserve memory
// The structure of this log can be changed as new outputs are required
// Currently, it records the initial strategies, list of strategies per round and the population utility per round
// plus the full list of interaction records per round if debug is on
case class SimulationRecord (configuration: Configuration, initialStrategies: Vector[Action], strategies: Vector[Vector[Action]],
                             roundUtility: Vector[Double], interactions: Option[Vector[Vector[InteractionRecord]]])
