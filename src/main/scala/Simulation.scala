import scala.util.Random.{shuffle, nextDouble => randomDouble, nextInt => randomInt}
import Utilities._

object Simulation extends App {
  import Networks._
  import Logging._

  val configuration = ConfigurationA

  import configuration._

  // Neighbourhood links between agents
  val network: Vector[(Agent, Agent)] = logIfExtreme (logNetwork) (networkType match {
    case SmallWorldNetwork => smallWorldNetwork (numberOfAgents, averageDegree, nonLatticeProbability)
    case ScaleFreeNetwork => scaleFreeNetwork (numberOfAgents)
    case FullyConnectedNetwork => fullyConnectedNetwork (numberOfAgents)
  })
  // List of agents in network
  val agents: Vector[Agent] =
    nodes (network)
  // Map of each agent to a list of its neighbours
  val neighbours: Map[Agent, Vector[Agent]] =
    agents.createMap (agent => connected (agent, network))
  // Choose a random neighbour of the given agent
  def randomNeighbour (agent: Agent): Agent =
    randomChoice (neighbours (agent))

  // An action, represented by its ID
  case class Action (id: Int) extends AnyVal {
    override def toString: String = s"Act$id"
  }
  // List of possible actions, just integers from 0 to numberOfActions - 1
  val actions: Vector[Action] =
    (0 until numberOfActions).toVector.map (Action)
  // Get a random possible action
  def randomAction: Action =
    Action (randomInt (numberOfActions))
  // Create an agent's reward vector where the utility of the given ideal action is 1.0 tailoring linearly to minActionReward
  //   on either side
  def actionRewardVector (ideal: Action): Map[Action, Double] =
    actions.createMap (action =>
      (1.0 + (if (action.id >= ideal.id) ideal.id - action.id else action.id - ideal.id) * 2.0 / numberOfActions).max (minActionReward))
  // Map of each agent to its action reward vector
  val actionReward: Map[Agent, Map[Action, Double]] = logIfExtreme (logActionRewards) (
    agents.createMap (agent => actionRewardVector (Action (randomInt (numberOfActions))))
  )

  if (aggregateLog) println (logPopulationRewards (actionReward) + "\n")

  // The function returning the coordination matrix value given the distance from the main diagonal,
  //   distance varies from 0.0 on the diagonal to 1.0 at the extreme corners
  def miscoordination (distanceFromDiagonal: Double): Double =
    if (absoluteCoordinationCost)
      if (distanceFromDiagonal == 0.0) 1.0 else 0.0
    else
      1.0 - distanceFromDiagonal
  // The coordination reward matrix, maps a pair of actions to a utility multiplier
  val coordinationReward: Map[(Action, Action), Double] =
    (for (x <- actions; y <- actions) yield {
      val distance = (x.id - y.id).abs.toDouble / (numberOfActions - 1)
      (x, y) -> miscoordination (distance)
    }).toMap

  if (aggregateLog) println (logCoordinationRewards (coordinationReward, actions) + "\n")

  // Calculate one agent's reward from an interaction between itself and another agent, each performing given actions
  def interactionReward (agent: Agent, action: Action, othersAction: Action): Double =
    actionReward (agent)(action) * coordinationReward (action, othersAction)
  // A record of the interaction between two agents, giving their actions and the instigator's reward
  case class InteractionRecord (round: Int, instigatorAgent: Agent, receiverAgent: Agent,
                                instigatorAction: Action, receiverAction: Action,
                                instigatorReward: Double, receiverReward: Double) {
    override def toString =
      s"$round: [$instigatorAgent:$instigatorAction]($instigatorReward) <=> [$receiverAgent:$receiverAction]($receiverReward)"
  }
  // Execute an interaction between two agents performing actions, returning the interaction record
  def interact (round: Int, instigatorAgent: Agent, receiverAgent: Agent, instigatorAction: Action, receiverAction: Action): InteractionRecord =
    InteractionRecord (round, instigatorAgent, receiverAgent, instigatorAction, receiverAction,
      interactionReward (instigatorAgent, instigatorAction, receiverAction),
      interactionReward (receiverAgent, receiverAction, instigatorAction))

  // Return all interactions from the given set which the given agent observed, i.e. was a participant
  def observedInteractions (observerAgent: Agent, records: Vector[InteractionRecord]): Vector[InteractionRecord] =
    records.filter (record => record.instigatorAgent == observerAgent || record.receiverAgent == observerAgent)
  // Calculate the mean average of a list of values
  def mean (values: Vector[Double]): Double =
    values.sum / values.size
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

  // Evaluate the average reward this agent would receive if the population committed to each of the proposed strategies
  def proposalEvaluations (agent: Agent, proposals: Set[Action]): Map[Action, Double] =
    proposals.createMap (strategy => actionReward (agent)(strategy))

  // The log of results of the simulation, aggregated as we go along to conserve memory
  // The structure of this log can be changed as new outputs are required
  // Currently, it records the initial strategies, list of strategies per round and the population utility per round
  // plus the full list of interaction records per round if debug is on
  case class SimulationRecord (configuration: Configuration, initialStrategies: Vector[Action], strategies: Vector[Vector[Action]],
                               roundUtility: Vector[Double], interactions: Option[Vector[Vector[InteractionRecord]]])
  // The empty simulation record, to be added to as the simulation runs
  // This can be adjusted if SimulationRecord is changed to add new fields
  def emptySimulationRecord (initialStrategies: Map[Agent, Action]): SimulationRecord =
    SimulationRecord (configuration, initialStrategies.valuesIterator.toVector, Vector.empty, Vector.empty,
      if (aggregateLog) Some (Vector.empty) else None)
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

  // Simulate from the given round until numberOfRounds, given the current strategy of each agent, the interaction
  // history within the memory window, number of re-evaluations maintaining the same strategy, and the running results,
  // and returning the completed simulation results.
  def simulateFromRound (round: Int, strategy: Map[Agent, Action], history: Vector[Vector[InteractionRecord]],
                         consistency: Map[Agent, Int], committing: Boolean, results: SimulationRecord): SimulationRecord = {
    if (round == numberOfRounds) results
    else {
      if (extremeLog) println (s"Round $round")
      // Map agents to the action they perform this round, either their strategy or a random exploration if not last round
      val roundAction = logIfExtreme (logActionsChosen) (agents.createMap (agent =>
        if (round < numberOfRounds - 1 && randomDouble < explorationProbability) randomAction else strategy (agent)))
      // Perform interactions by all agents for this round, returning the interaction records
      val interactions: Vector[InteractionRecord] = logIfExtreme (logInteractions) (
        for (instigator <- agents; _ <- 1 to interactionsInstigatedPerRound) yield {
          val receiver = randomNeighbour (instigator)
          interact (round, instigator, receiver, roundAction (instigator), roundAction (receiver))
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
      val proposals: Set[Action] = if (committing && round % copyFrequency == 0) {
        newConsistency.filter (a => a._2 == proposalIterations).map (a => strategy (a._1)).toSet
      } else Set.empty
      if (proposals.nonEmpty) println (s"Proposals: $proposals")
      // Each agent's evaluation of the value of each proposal
      val evaluations: Map[Agent, Map[Action, Double]] =
        agents.createMap (agent => proposalEvaluations (agent, proposals))
      // Map each agent to the actions it would vote in favour of committing to
      val inFavour: Map[Agent, Set[Action]] =
        evaluations.transformValues ((agent, values) =>
        values.filter (eval => eval._2 > ratingPerAction (agent).getOrElse (eval._1, 0.0)).keySet)
      // Map each proposal to the number of votes cast
      val votes: Map[Action, Int] =
        proposals.createMap (proposal => inFavour.count (a => a._2.contains (proposal)))
      if (votes.nonEmpty) println (s"Votes: $votes")
      // A proposal winning over half the votes, if any
      val winner: Option[Action] =
        shuffle (votes.filter (_._2 > numberOfAgents / 2).keys.toVector).headOption
      if (winner.nonEmpty) println (s"Winner: ${winner.get}")

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

      simulateFromRound (round + 1, newStrategy, newHistory, newConsistency, committing, newResults)
    }
  }

  // Perform one simulation of the above defined environment
  def simulate (committing: Boolean): SimulationRecord = {
    // Initial strategy of each agent, random actions
    val initialStrategy: Map[Agent, Action] = agents.createMap (agent => randomAction)
    // Simulate from round 0, starting with no interaction history and an empty simulation record
    simulateFromRound (round = 0, initialStrategy, Vector.empty, agents.createMap (_ => 0), committing,
      emptySimulationRecord (initialStrategy))
  }

  // Run a set of simulations in parallel, capture the results
  val recordNoCommitting: Vector[SimulationRecord] =
    (0 until numberOfSimulations).par.map {_ => simulate (committing = false)}.seq.toVector
  val recordCommitting: Vector[SimulationRecord] =
    (0 until numberOfSimulations).par.map {_ => simulate (committing = true)}.seq.toVector

  //plotConvergence (record)

  if (aggregateLog) {
    println (logTopStrategiesPerRound (recordNoCommitting, 5, 5, numberOfRounds) + "\n")
    //println (logObservedStrategyUtilitiesPerRound (recordNoCommitting, 5, 5, numberOfRounds, agents) + "\n")
    println (logFinalStrategyDistribution (recordNoCommitting) + "\n")
  }

  // Returns true if any strategy has converged given the list of strategies agents have in a round
  def hasConverged (strategies: Vector[Action]): Boolean =
    strategies.distinct.exists (
      norm => (strategies.count (_ == norm).toDouble / strategies.size) > convergencePercentage
    )
  // Returns the round in the given results where the population first converges to a norm, or -1 if no convergence
  def firstConverge (results: SimulationRecord): Int =
    results.strategies.indexWhere (hasConverged)
  // A map of simulation records to the round where they first converged, excluding simulations that did not converge
  val convergingNoCommitting: Vector[(SimulationRecord, Int)] =
    recordNoCommitting.map (sim => (sim, firstConverge (sim))).filter (_._2 != -1)
  val convergingCommitting: Vector[(SimulationRecord, Int)] =
    recordCommitting.map (sim => (sim, firstConverge (sim))).filter (_._2 != -1)
  // Calculate the cumulative utility over the given rounds (from <= round < until) of a simulation
  def cumulativeUtility (results: SimulationRecord, from: Int, until: Int): Double =
    results.roundUtility.slice (from, until).sum

  // Print percentage of simulations that converged to a norm
  println (s"Simulations converging to a norm (no committing): ${convergingNoCommitting.size.toDouble / numberOfSimulations * 100}%")
  println (s"Simulations converging to a norm (committing): ${convergingCommitting.size.toDouble / numberOfSimulations * 100}%")
  // Print round that simulations first converged to norm, on average
  println (s"Average round first converged (no committing): ${mean (convergingNoCommitting.map (_._2.toDouble)).toInt} of $numberOfRounds")
  println (s"Average round first converged (committing): ${mean (convergingCommitting.map (_._2.toDouble)).toInt} of $numberOfRounds")
  // Print cumulative utility for whole simulation duration, averaged over all simulations
  println (s"Total cumulative utility (no committing): ${mean (recordNoCommitting.map (result => cumulativeUtility (result, 0, numberOfRounds)))}")
  println (s"Total cumulative utility (committing): ${mean (recordCommitting.map (result => cumulativeUtility (result, 0, numberOfRounds)))}")
  // Print cumulative utility for whole simulation duration prior to convergence, averaged over all converging simulations
  println (s"Cumulative utility prior to convergence (no committing): ${mean (convergingNoCommitting.map (result => cumulativeUtility (result._1, 0, result._2)))}")
  println (s"Cumulative utility prior to convergence (committing): ${mean (convergingCommitting.map (result => cumulativeUtility (result._1, 0, result._2)))}")
  // Print cumulative utility for whole simulation duration after convergence, averaged over all converging simulations
  println (s"Cumulative utility after convergence (no committing): ${mean (convergingNoCommitting.map (result => cumulativeUtility (result._1, result._2, numberOfRounds)))}")
  println (s"Cumulative utility after convergence (committing): ${mean (convergingCommitting.map (result => cumulativeUtility (result._1, result._2, numberOfRounds)))}")
}

