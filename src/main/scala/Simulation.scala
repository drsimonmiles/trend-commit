import scala.util.Random.{nextDouble => randomDouble, nextInt => randomInt}

object Simulation extends App {
  import Networks._
  import Logging._

  // Network generation parameters
  val numberOfAgents = 500
  val useSmallWorldNetwork = false    // false = scale-free, true = small world
  val averageDegree = 4               // parameter for small world network: should be even
  val nonLatticeProbability = 0.4     // parameter for small world network generation (beta parameter)
  // Other model parameters
  val numberOfActions = 40
  val interactionsInstigatedPerRound = 4     // Number of interactions each agent instigates per round
  val explorationProbability = 0.1           // Likelihood of an agent trying a random action for a round
  val copyFrequency = 1                      // How many rounds between agents copying each others' best strategies
  val absoluteCoordinationCost = true        // true = non-matching actions produce utility 0, false = decreasing utility as more different
  // Simulation parameters
  val numberOfRounds = 3000
  val numberOfSimulations = 1
  val convergencePercentage = 0.95     // The percentage of agents with same strategy required for convergence

  // List of agents, just integers from 0 to numberOfAgents - 1
  val agents: Vector[Agent] =
    (0 until numberOfAgents).toVector.map (new Agent (_))
  // Neighbourhood links between agents
  val network: Vector[(Agent, Agent)] =
    if (useSmallWorldNetwork) smallWorldNetwork (numberOfAgents, averageDegree, nonLatticeProbability)
    else scaleFreeNetwork (numberOfAgents)
  // Map of each agent to a list of its neighbours
  val neighbours: Map[Agent, Vector[Agent]] =
    agents.map (agent => agent -> connected (agent, network)).toMap
  // Choose a random neighbour of the given agent
  def randomNeighbour (agent: Agent): Agent = {
    val neighbourhood = neighbours (agent)
    neighbourhood (randomInt (neighbourhood.size))
  }

  class Action (val id: Int) extends AnyVal {
    override def toString: String = s"Act$id"
  }
  // List of possible actions, just integers from 0 to numberOfActions - 1
  val actions: Vector[Action] =
    (0 until numberOfActions).toVector.map (new Action (_))
  def randomAction: Action =
    new Action (randomInt (numberOfActions))
  // Create an agent's reward vector where the utility of the given ideal action is 1.0 tailoring linearly to 0.0
  //   on either side
  def actionRewardVector (ideal: Action): Map[Action, Double] =
    actions.map (action =>
      action -> (1.0 + (if (action.id >= ideal.id) ideal.id - action.id else action.id - ideal.id) * 2.0 / numberOfActions).max (0.0)).toMap
  // Map of each agent to its action reward vector
  val actionReward: Map[Agent, Map[Action, Double]] =
    agents.map (agent => agent -> actionRewardVector (new Action (randomInt (numberOfActions)))).toMap

  println (logPopulationRewards (actionReward) + "\n")

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

  println (logCoordinationRewards (coordinationReward, actions) + "\n")

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
  // Return the action producing the best average reward as recorded in the given interaction records
  def bestStrategy (records: Vector[InteractionRecord]): Action = {
    // List of tuples of (action, reward) for instigators of the given interactions
    val rewardsPerInstigatorAction = records.map (record => (record.instigatorAction, record.instigatorReward))
    // List of tuples of (action, reward) for receivers of the given interactions
    val rewardsPerReceiverAction = records.map (record => (record.receiverAction, record.receiverReward))
    // Map of action to the list of (action, reward) tuples for both instigators and receivers
    val rewardsPerAction = (rewardsPerInstigatorAction ++ rewardsPerReceiverAction).groupBy (_._1)
    // Calculate the mean reward per action, and return the action with the maximum reward
    rewardsPerAction.mapValues (records => mean (records.map (_._2))).maxBy (_._2)._1
  }

  // The log of results of the simulation, aggregated as we go along to conserve memory
  // The structure of this log can be changed as new outputs are required
  // Currently, it records the list of strategies per round and the population utility per round
  case class SimulationRecord (strategies: Vector[Vector[Action]], roundUtility: Vector[Double])
  // The empty simulation record, to be added to as the simulation runs
  // This can be adjusted if SimulationRecord is changed to add new fields
  val emptySimulationRecord: SimulationRecord = SimulationRecord (Vector.empty, Vector.empty)
  // Aggregates the full data from a round into the running simulation record, given the strategy per agent that round
  //  and records of all interactions that round
  // The strategy parameter is a mapping of agents to their strategies this round
  // This function's implementation can be altered along with SimulationRecord to accumulate other data as needed
  def aggregateRoundResults (previous: SimulationRecord, strategy: Map[Agent, Action],
                             roundInteractions: Vector[InteractionRecord]): SimulationRecord =
    SimulationRecord (
      previous.strategies :+ strategy.valuesIterator.toVector,
      previous.roundUtility :+ roundInteractions.map (
        interaction => interaction.instigatorReward + interaction.receiverReward).sum)

  // Perform one simulation of the above defined environment
  def simulate (): SimulationRecord = {
    // Current strategy of each agent, initialise to random actions
    var strategy: Map[Agent, Action] =
      agents.map (agent => agent -> randomAction).toMap
    println (strategy.groupBy (_._2).mapValues (_.size).toVector.sortBy (_._2))
    // The memorised interaction records of the agents, for the last copyFrequency rounds
    var history = Vector[Vector[InteractionRecord]] ()
    // The running results from this simulation run
    var results = emptySimulationRecord

    for (round <- 0 until numberOfRounds) {
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
      history = interactions +: (if (history.size < copyFrequency) history else history.init)
      // Every copyFrequency rounds, each agent copies the best strategy in the interactions they've observed
      if (round % copyFrequency == 0) {
        // Get the history of interaction records as a single set, rather than per round
        val allHistory = history.flatten
        // Set the strategy for each agent to be the best of those they've observed from the remembered history
        strategy = agents.map (agent => agent -> bestStrategy (observedInteractions (agent, allHistory))).toMap

        if (round >= 0 && round < 5) println (strategy.groupBy (_._2).mapValues (_.size).toVector.sortBy (_._2))
      }
      // Aggregate the round's results into the running results object
      results = aggregateRoundResults (results, strategy, interactions)
    }
    // Return the simulation results
    results
  }

  // Run a set of simulations in parallel, capture the results
  val record: Vector[SimulationRecord] =
    (0 until numberOfSimulations).par.map {_ => simulate ()}.seq.toVector

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
  println (record.head.strategies.last.take (50))
  println (record.head.strategies.last.distinct)
  println (record.head.strategies.last.distinct.map (n => record.head.strategies.last.count (_ == n)).sorted.reverse)
  println (record.head.strategies.last.distinct.map (n => record.head.strategies.last.count (_ == n).toDouble / record.head.strategies.last.size).sorted.reverse)

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

/**
  * Algorithms to generate networks, and utility functions to query them.
  * Networks are structured as a set of (Int, Int) tuples representing each undirected edge between two
  * nodes. The nodes are integers from 0 to numberOfNodes.
  */
object Networks {
  class Agent (val id: Int) extends AnyVal {
    override def toString: String = s"Ag$id"
  }

  /** Generate a scale-free network with a given number of nodes using Barabási-Albert model. */
  def scaleFreeNetwork (numberOfNodes: Int): Vector[(Agent, Agent)] = {
    // Given a set of edges created so far and the degrees of nodes added so far, add the rest of the nodes
    def addNodes (edgesSoFar: Vector[(Agent, Agent)], nodeDegrees: Vector[Int], degreeSum: Int): Vector[(Agent, Agent)] =
      // If the number of nodes we've added is the total, return the network
      if (nodeDegrees.size == numberOfNodes) edgesSoFar
      else {
        // Sum the degrees of all nodes, choose a random value from 1 to that sum
        // Treat this value like an index to find an existing node which the new node is then connected to
        // The index locates the node by deducting each node degree until the index reduces to/below zero
        // This is equivalent to selecting a node with a probability based on its node degree
        def locateNode (position: Int, fromIndex: Int): Int =
          if (position - nodeDegrees (fromIndex) <= 0) fromIndex
          else locateNode (position - nodeDegrees (fromIndex), fromIndex + 1)
        val connectTo = locateNode (randomInt (degreeSum) + 1, 0)
        // Update the node degrees to account for the new edge and add the remaining nodes
        val newDegrees = 1 +: nodeDegrees.updated (connectTo, nodeDegrees (connectTo) + 1)
        addNodes ((new Agent (nodeDegrees.size), new Agent (connectTo)) +: edgesSoFar, newDegrees, degreeSum + 2)
      }
    // Start with the first two nodes and their connecting edge in place and then add the rest
    addNodes (Vector ((new Agent (0), new Agent (1))), Vector (1, 1), 2)
  }

  /** Generate a small world network with a given number of nodes, average degree and
    * non-lattice probability (beta parameter) using Watts-Strogatz model */
  def smallWorldNetwork (numberOfNodes: Int, averageDegree: Int, nonLatticeProbability: Double): Vector[(Agent, Agent)] = {
    // Generate a regular ring lattice as a starting point
    val lattice = ringLattice (numberOfNodes, averageDegree)
    // Convenience function to return a value unchanged if below a threshold, value+1 if over the threshold
    def incrementIfOver (value: Int, threshold: Int): Int = if (value < threshold) value else value + 1
    // Returns a random node excluding the given node
    def randomNodeExcluding (exclude: Int): Int = incrementIfOver (randomInt (numberOfNodes - 1), exclude)
    // Returns true if the neighbour is within averageDegree/2 to the right of node around the ring
    def withinDistanceToRight (neighbour: Int, node: Int): Boolean =
      (if (neighbour > node) neighbour else neighbour + numberOfNodes) <= node + averageDegree / 2

    // For each node, replace each edge it has to a node within averageDistance/2 around the ring, with
    // a probability of nonLatticeProbability, with an edge to a random other node
    (for (node <- 0 until numberOfNodes; neighbour <- connected (new Agent (node), lattice)) yield {
      if (withinDistanceToRight (neighbour.id, node) && randomDouble () < nonLatticeProbability)
        (new Agent (node), new Agent (randomNodeExcluding (node)))
      else
        (new Agent (node), neighbour)
    }).toVector
  }

  /** Generate a regular ring lattice with a given number of nodes each with a given number of neighbours.
    * The node IDs will go around the ring in continguous numerical order. */
  def ringLattice (numberOfNodes: Int, numberOfNeighbours: Int): Vector[(Agent, Agent)] =
    (for (nodeA <- 0 until numberOfNodes; nodeB <- 0 until numberOfNodes) yield {
      val order = (nodeA - nodeB).abs % (numberOfNodes - 1 - (numberOfNeighbours / 2))
      if (order > 0 && order <= numberOfNeighbours / 2) Vector ((new Agent (nodeA), new Agent (nodeB)))
      else Vector.empty
    }).flatten.toVector

  /** Return the nodes in a (sub-)network for which the edges between nodes are given */
  def nodes (network: Vector[(Agent, Agent)]): Vector[Agent] =
    network.flatMap (edge => Vector (edge._1, edge._2)).distinct

  /** Return the nodes connected to the given node from the given network */
  def connected (node: Agent, network: Vector[(Agent, Agent)]): Vector[Agent] =
    nodes (network.filter (edge => edge._1 == node || edge._2 == node)).filter (_ != node)
}

object Logging {
  import Networks.Agent
  import Simulation.Action

  def populationRewards (rewards: Map[Agent, Map[Action, Double]]): Map[Action, Double] =
    rewards.values.flatten.groupBy (_._1).mapValues (_.foldLeft (0.0)((sum, reward) => sum + reward._2))

  def logPopulationRewards (rewards: Map[Agent, Map[Action, Double]]): String =
    populationRewards (rewards).toVector.sortBy (_._1.id).map (r => f"${r._1}: ${r._2}%1.0f").mkString ("Population rewards: ", ", ", "\n") +
      populationRewards (rewards).toVector.sortBy (_._2).reverse.map (r => f"${r._1}: ${r._2}%1.0f").mkString ("Ordered by reward: ", ", ", "")

  def logCoordinationRewards (matrix: Map[(Action, Action), Double], actions: Vector[Action]): String =
    "Coordination reward matrix:\n" +
      (for (row <- actions) yield
        (for (col <- actions) yield
          f"${matrix ((row, col))}%1.1f").mkString (" ")).mkString ("\n")
}