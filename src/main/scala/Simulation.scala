import scala.util.Random.{nextDouble => randomDouble, nextInt => randomInt, shuffle}

object Simulation extends App {
  import Networks._
  import Logging._

  val configuration = ConfigurationB
  val debug = true

  import configuration._

  // Neighbourhood links between agents
  val network: Vector[(Agent, Agent)] = networkType match {
    case SmallWorldNetwork => smallWorldNetwork (numberOfAgents, averageDegree, nonLatticeProbability)
    case ScaleFreeNetwork => scaleFreeNetwork (numberOfAgents)
    case FullyConnectedNetwork => fullyConnectedNetwork (numberOfAgents)
  }
  // List of agents in network
  val agents: Vector[Agent] =
    nodes (network)
  // Map of each agent to a list of its neighbours
  val neighbours: Map[Agent, Vector[Agent]] =
    agents.map (agent => agent -> connected (agent, network)).toMap
  // Choose a random neighbour of the given agent
  def randomNeighbour (agent: Agent): Agent = {
    val neighbourhood = neighbours (agent)
    neighbourhood (randomInt (neighbourhood.size))
  }

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
  // Create an agent's reward vector where the utility of the given ideal action is 1.0 tailoring linearly to 0.0
  //   on either side
  def actionRewardVector (ideal: Action): Map[Action, Double] =
    actions.map (action =>
      action -> (1.0 + (if (action.id >= ideal.id) ideal.id - action.id else action.id - ideal.id) * 2.0 / numberOfActions).max (0.0)).toMap
  // Map of each agent to its action reward vector
  val actionReward: Map[Agent, Map[Action, Double]] =
    agents.map (agent => agent -> actionRewardVector (Action (randomInt (numberOfActions)))).toMap

  if (debug) println (logPopulationRewards (actionReward) + "\n")

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

  if (debug) println (logCoordinationRewards (coordinationReward, actions) + "\n")

  // Calculate one agent's reward from an interaction between itself and another agent, each performing given actions
  def interactionReward (agent: Agent, action: Action, othersAction: Action): Double =
    actionReward (agent)(action) * coordinationReward (action, othersAction)
  // A record of the interaction between two agents, giving their actions and the instigator's reward
  case class InteractionRecord (instigatorAgent: Agent, receiverAgent: Agent,
                                instigatorAction: Action, receiverAction: Action,
                                instigatorReward: Double, receiverReward: Double)
  // Execute an interaction between two agents performing actions, returning the interaction record
  def interact (instigatorAgent: Agent, receiverAgent: Agent, instigatorAction: Action, receiverAction: Action): InteractionRecord =
    InteractionRecord (instigatorAgent, receiverAgent, instigatorAction, receiverAction,
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
  // Return the action producing the best average reward as recorded in the given interaction records
  def bestStrategy (records: Vector[InteractionRecord]): Action = {
    val best = shuffle (strategyRatings (records).toVector).maxBy (_._2)._1
//    if (strategyRatings (records)(best) > 0.0) println (best + " <- " + records)
    best
  }

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
      if (debug) Some (Vector.empty) else None)
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
  // history within the memory window, and the running results, and returning the completed simulation results.
  def simulateFromRound (round: Int, strategy: Map[Agent, Action], history: Vector[Vector[InteractionRecord]],
                         results: SimulationRecord): SimulationRecord = {
    if (round == numberOfRounds) results
    else {
      // Map agents to the action they perform this round, either their strategy or a random exploration
      val roundAction = agents.map (agent => agent ->
        (if (randomDouble < explorationProbability) randomAction else strategy (agent))).toMap
      // Perform interactions by all agents for this round, returning the interaction records
      val interactions: Vector[InteractionRecord] =
        for (instigator <- agents; _ <- 1 to interactionsInstigatedPerRound) yield {
          val receiver = randomNeighbour (instigator)
          interact (instigator, receiver, roundAction (instigator), roundAction (receiver))
        }
      // Add the current round interaction records to the history, removing the oldest
      val newHistory = interactions +: (if (history.size < copyFrequency) history else history.init)
      // Every copyFrequency rounds, each agent copies the best strategy in the interactions they've observed
      val newStrategy = if (round % copyFrequency == 0) {
        // Get the history of interaction records as a single set, rather than per round
        val allHistory = newHistory.flatten
        // Set the strategy for each agent to be the best of those they've observed from the remembered history
        agents.map (agent => agent -> bestStrategy (observedInteractions (agent, allHistory))).toMap
        //strategyRatingsForDebug (observedInteractions (agent, allHistory))
      } else strategy
      // Aggregate the round's results into the running results object
      val newResults = aggregateRoundResults (results, strategy, interactions)

      simulateFromRound (round + 1, newStrategy, newHistory, newResults)
    }
  }

  // Perform one simulation of the above defined environment
  def simulate (): SimulationRecord = {
    // Initial strategy of each agent, random actions
    val initialStrategy: Map[Agent, Action] = agents.map (agent => agent -> randomAction).toMap
    // Simulate from round 0, starting with no interaction history and an empty simulation record
    simulateFromRound (round = 0, initialStrategy, Vector.empty, emptySimulationRecord (initialStrategy))
  }

  // Run a set of simulations in parallel, capture the results
  val record: Vector[SimulationRecord] =
    (0 until numberOfSimulations).par.map {_ => simulate ()}.seq.toVector

  plotConvergence (record.head)

  if (debug) {
    println (logTopStrategiesPerRound (record, 5, 5, numberOfRounds) + "\n")
    println (logObservedStrategyUtilitiesPerRound (record, 5, 5, numberOfRounds, agents) + "\n")
    println (logFinalStrategyDistribution (record) + "\n")
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
  val converging: Vector[(SimulationRecord, Int)] =
    record.map (sim => (sim, firstConverge (sim))).filter (_._2 != -1)
  // Calculate the cumulative utility over the given rounds (from <= round < until) of a simulation
  def cumulativeUtility (results: SimulationRecord, from: Int, until: Int): Double =
    results.roundUtility.slice (from, until).sum

  // Temporary logging for debugging
//  println (record.head.strategies.last.take (50))
//  println (record.head.strategies.last.distinct)
//  println (record.head.strategies.last.distinct.map (n => record.head.strategies.last.count (_ == n)).sorted.reverse)
//  println (record.head.strategies.last.distinct.map (n => record.head.strategies.last.count (_ == n).toDouble / record.head.strategies.last.size).sorted.reverse)

  // Print percentage of simulations that converged to a norm
  println (s"Simulations converging to a norm: ${converging.size.toDouble / numberOfSimulations * 100}%")
  // Print round that simulations first converged to norm, on average
  println (s"Round first converged: ${mean (converging.map (_._2.toDouble))}")
  // Print cumulative utility for whole simulation duration, averaged over all simulations
  println (s"Total cumulative utility: ${mean (record.map (result => cumulativeUtility (result, 0, numberOfRounds)))}")
  // Print cumulative utility for whole simulation duration prior to convergence, averaged over all converging simulations
  println (s"Cumulative utility prior to convergence: ${mean (converging.map (result => cumulativeUtility (result._1, 0, result._2)))}")
  // Print cumulative utility for whole simulation duration after convergence, averaged over all converging simulations
  println (s"Cumulative utility after convergence: ${mean (converging.map (result => cumulativeUtility (result._1, result._2, numberOfRounds)))}")
}

