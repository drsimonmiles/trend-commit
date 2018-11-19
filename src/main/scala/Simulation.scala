import scala.util.Random.{shuffle, nextDouble => randomDouble, nextInt => randomInt}
import Networks._
import Logging._
import Utilities._

object Simulation {
  // Choose a random neighbour of the given agent
  def randomNeighbour (agent: Agent, neighbours: Map[Agent, Vector[Agent]]): Agent =
    randomChoice (neighbours (agent))

  // Get a random possible action
  def randomAction (numberOfActions: Int): Action =
    Action (randomInt (numberOfActions))

  // Create an agent's reward vector where the utility of the given ideal action is 1.0 tailoring linearly to minActionReward
  //   on either side
  def actionRewardVector (ideal: Action, actions: Vector[Action], minActionReward: Double): Map[Action, Double] =
    actions.createMap (action =>
      (1.0 + (if (action.id >= ideal.id) ideal.id - action.id else action.id - ideal.id) * 2.0 / actions.size).max (minActionReward))

  // The function returning the coordination matrix value given the distance from the main diagonal,
  //   distance varies from 0.0 on the diagonal to 1.0 at the extreme corners
  def miscoordination (distanceFromDiagonal: Double, absoluteCoordinationCost: Boolean): Double =
    if (absoluteCoordinationCost)
      if (distanceFromDiagonal == 0.0) 1.0 else 0.0
    else
      1.0 - distanceFromDiagonal

  // Calculate one agent's reward from an interaction between itself and another agent, each performing given actions
  def interactionReward (agent: Agent, action: Action, othersAction: Action, actionReward: Map[Agent, Map[Action, Double]], coordinationReward: Map[(Action, Action), Double]): Double =
    actionReward (agent)(action) * coordinationReward (action, othersAction)

  // Execute an interaction between two agents performing actions, returning the interaction record
  def interact (round: Int, instigatorAgent: Agent, receiverAgent: Agent, instigatorAction: Action, receiverAction: Action,
                actionReward: Map[Agent, Map[Action, Double]], coordinationReward: Map[(Action, Action), Double]): InteractionRecord =
    InteractionRecord (round, instigatorAgent, receiverAgent, instigatorAction, receiverAction,
      interactionReward (instigatorAgent, instigatorAction, receiverAction, actionReward, coordinationReward),
      interactionReward (receiverAgent, receiverAction, instigatorAction, actionReward, coordinationReward))

  // Return all interactions from the given set which the given agent observed, i.e. was a participant
  def observedInteractions (observerAgent: Agent, records: Vector[InteractionRecord]): Vector[InteractionRecord] =
    records.filter (record => record.instigatorAgent == observerAgent || record.receiverAgent == observerAgent)

  // Extract the actions performed by instigators and the rewards received by them for each interaction in set
  def instigatorRewards (records: Vector[InteractionRecord]): Vector[(Action, Double)] =
    records.map (record => (record.instigatorAction, record.instigatorReward))

  // Extract the actions performed by receivers and the rewards received by them for each interaction in set
  def receiverRewards (records: Vector[InteractionRecord]): Vector[(Action, Double)] =
    records.map (record => (record.receiverAction, record.receiverReward))

  // Extract the actions performed either party and the rewards received by them for each interaction in set
  def actionRewards (records: Vector[InteractionRecord]): Map[Action, Vector[Double]] =
    (instigatorRewards (records) ++ receiverRewards (records)).groupBy (_._1).mapValues (_.map (_._2))

  // Return the mean average reward received from each action by agents in the interaction set
  def strategyRatings (records: Vector[InteractionRecord]): Map[Action, Double] =
    actionRewards (records).mapValues (mean)

  // Return the action producing the best average reward as recorded in the given interaction records,
  //   shuffling records first so that if records all have the same utility, different ones are picked each time
  def bestStrategy (ratings: Map[Action, Double]): Action =
    shuffle (ratings.toVector).maxBy (_._2)._1

  // The empty simulation record, to be added to as the simulation runs
  // This can be adjusted if SimulationRecord is changed to add new fields
  def emptySimulationRecord (initialStrategies: Map[Agent, Action], actionRewards: Map[Agent, Map[Action, Double]], configuration: Configuration): SimulationRecord =
    SimulationRecord (initialStrategies.valuesIterator.toVector, actionRewards, Vector.empty, Vector.empty,
      if (configuration.aggregateLog) Some (Vector.empty) else None)

  // Aggregates the full data from a round into the running simulation record, given the strategy per agent that round
  //  and records of all interactions that round
  // The strategy parameter is a mapping of agents to their strategies this round
  // This function's implementation can be altered along with SimulationRecord to accumulate other data as needed
  def aggregateRoundResults (previous: SimulationRecord, strategy: Map[Agent, Action],
                             roundInteractions: Vector[InteractionRecord]): SimulationRecord =
    previous.copy (
      strategies = previous.strategies :+ strategy.valuesIterator.toVector,
      roundUtility = previous.roundUtility :+ roundInteractions.map (
        interaction => interaction.instigatorReward + interaction.receiverReward).sum,
      interactions = previous.interactions.map (_ :+ roundInteractions))

  // Evaluate the average reward this agent would receive if the population committed to each of the proposed strategies
  def proposalEvaluations (agent: Agent, proposals: Set[Action], actionReward: Map[Agent, Map[Action, Double]]): Map[Action, Double] =
    proposals.createMap (strategy => actionReward (agent)(strategy))

  // Perform one simulation of the above defined environment
  def simulateOnce (configuration: Configuration, agents: Vector[Agent], neighbours: Map[Agent, Vector[Agent]],
                    actionReward: Map[Agent, Map[Action, Double]], coordinationReward: Map[(Action, Action), Double]): SimulationRecord = {
    import configuration._

    // Simulate from the given round until numberOfRounds, given the current strategy of each agent, the interaction
    // history within the memory window, number of re-evaluations maintaining the same strategy, and the running results,
    // and returning the completed simulation results.
    def simulateFromRound (round: Int, strategy: Map[Agent, Action], history: Vector[Vector[InteractionRecord]],
                           consistency: Map[Agent, Int], results: SimulationRecord): SimulationRecord = {
      if (round == numberOfRounds) results
      else {
        if (extremeLog) println (s"Round $round")
        // Map agents to the action they perform this round, either their strategy or a random exploration if not last round
        val roundAction = logIfExtreme (logActionsChosen) (agents.createMap (agent =>
          if (round < numberOfRounds - 1 && randomDouble < explorationProbability) randomAction (numberOfActions) else strategy (agent)))
        // Perform interactions by all agents for this round, returning the interaction records
        val interactions: Vector[InteractionRecord] = logIfExtreme (logInteractions) (
          for (instigator <- agents; _ <- 1 to interactionsInstigatedPerRound) yield {
            val receiver = randomNeighbour (instigator, neighbours)
            interact (round, instigator, receiver, roundAction (instigator), roundAction (receiver), actionReward, coordinationReward)
          })
        // Add the current round interaction records to the history, removing the oldest
        val newHistory: Vector[Vector[InteractionRecord]] = logIfExtreme (logHistory) (
          interactions +: (if (history.size < copyFrequency) history else history.init)
        )
        // Each agent calculates the best strategy in the interactions they've observed
        if (extremeLog) println ("Choosing new strategies")
        // Get the history of interaction records as a single set, rather than per round
        val allHistory: Vector[InteractionRecord] =
          logIfExtreme (logFlatHistory) (newHistory.flatten)
        // Get the interactions each agent observed within the history
        val observedHistory: Map[Agent, Vector[InteractionRecord]] =
          logIfExtreme (logObservedHistory) (
            agents.createMap (agent => observedInteractions (agent, allHistory)))
        val ratingPerAction: Map[Agent, Map[Action, Double]] =
          logIfExtreme (logActionRatings) (
            agents.createMap (agent => strategyRatings (observedHistory (agent))))
        // Get the strategy for each agent that is the best of those they've observed from the remembered history
        val bestObservedStrategy: Map[Agent, Action] =
          logIfExtreme (logStrategyChosen) (agents.createMap (agent => bestStrategy (ratingPerAction (agent))))

        // Calculate for how many strategy re-evaluation points each agent has now kept the same strategy
        val newConsistency: Map[Agent, Int] =
          if (round % copyFrequency == 0) {
            consistency.transformValues ((agent, count) =>
              if (bestObservedStrategy (agent) == strategy (agent)) count + 1 else 0)
          } else consistency

        // The set of strategies to propose for mutual commitment
        val proposals: Set[Action] = if (mutualCommit && round % copyFrequency == 0) {
          newConsistency.filter (a => a._2 == proposalIterations).map (a => strategy (a._1)).toSet
        } else Set.empty
        // Each agent's evaluation of the value of each proposal
        val evaluations: Map[Agent, Map[Action, Double]] =
          agents.createMap (agent => proposalEvaluations (agent, proposals, actionReward))
        // Map each agent to the actions it would vote in favour of committing to
        val inFavour: Map[Agent, Set[Action]] =
          evaluations.transformValues ((agent, values) =>
            values.filter (eval => eval._2 > ratingPerAction (agent).getOrElse (eval._1, 0.0)).keySet)
        // Map each proposal to the number of votes cast
        val votes: Map[Action, Int] =
          proposals.createMap (proposal => inFavour.count (a => a._2.contains (proposal)))
        // A proposal winning over half the votes, if any
        val winner: Option[Action] =
          shuffle (votes.filter (_._2 > numberOfAgents / 2).keys.toVector).headOption

        // If there is a winning proposal, all strategies change to that.
        // Else every copyFrequenct rounds, each agent copies the best observed strategy.
        val newStrategy = winner match {
          case Some (commitment) => strategy.mapValues (_ => commitment)
          case None => if (round % copyFrequency == 0) bestObservedStrategy else strategy
        }

        // Aggregate the round's results into the running results object
        val newResults = logIfExtreme (logAggregatedResults) (
          aggregateRoundResults (results, strategy, interactions)
        )

        simulateFromRound (round + 1, newStrategy, newHistory, newConsistency, newResults)
      }
    }

    // Initial strategy of each agent, random actions
    val initialStrategy: Map[Agent, Action] = agents.createMap (agent => randomAction (numberOfActions))
    // Simulate from round 0, starting with no interaction history and an empty simulation record
    simulateFromRound (round = 0, initialStrategy, Vector.empty, agents.createMap (_ => 0),
      emptySimulationRecord (initialStrategy, actionReward, configuration))
  }

  def simulate (configuration: Configuration): AggregateRecord = {
    import configuration._

    // Neighbourhood links between agents
    val network: Vector[(Agent, Agent)] = logIfExtreme (logNetwork)(networkType match {
      case SmallWorldNetwork => smallWorldNetwork (numberOfAgents, averageDegree, nonLatticeProbability)
      case ScaleFreeNetwork => scaleFreeNetwork (numberOfAgents)
      case FullyConnectedNetwork => fullyConnectedNetwork (numberOfAgents)
    })
    // List of agents in network
    val agents: Vector[Agent] = nodes (network)
    // Map of each agent to a list of its neighbours
    val neighbours: Map[Agent, Vector[Agent]] = agents.createMap (agent => connected (agent, network))

    // List of possible actions, just integers from 0 to numberOfActions - 1
    val actions: Vector[Action] =
      (0 until numberOfActions).toVector.map (Action)
    // Map of each agent to its action reward vector
    val actionReward: Map[Agent, Map[Action, Double]] = logIfExtreme (logActionRewards)(
      agents.createMap (agent => actionRewardVector (Action (randomInt (numberOfActions)), actions, minActionReward))
    )

    if (aggregateLog) println (logPopulationRewards (actionReward) + "\n")

    // The coordination reward matrix, maps a pair of actions to a utility multiplier
    val coordinationReward: Map[(Action, Action), Double] =
      (for (x <- actions; y <- actions) yield {
        val distance = (x.id - y.id).abs.toDouble / (numberOfActions - 1)
        (x, y) -> miscoordination (distance, absoluteCoordinationCost)
      }).toMap

    if (aggregateLog) println (logCoordinationRewards (coordinationReward, actions) + "\n")

    // Run a set of simulations in parallel, return the results
    val runs = (0 until numberOfSimulations).par.map { _ =>
      simulateOnce (configuration, agents, neighbours, actionReward, coordinationReward) }.seq.toVector
    new AggregateRecord (configuration, runs)
  }
}
