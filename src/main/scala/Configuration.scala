sealed trait NetworkType
case object ScaleFreeNetwork extends NetworkType { override def toString = "scale-free" }
case object SmallWorldNetwork extends NetworkType { override def toString = "small world" }

case object FullyConnectedNetwork extends NetworkType { override def toString = "fully connected" }

sealed trait LoggingLevel
case object NoLogging extends LoggingLevel
case object AggregateLogging extends LoggingLevel
case object SimulationLogging extends LoggingLevel
case object ExtremeLogging extends LoggingLevel

object FixedConfiguration {
  val convergencePercentage = 0.95
}

case class Configuration ( // Network generation parameters
                           numberOfAgents: Int,
                           networkType: NetworkType, // small world, scale-free, or fully connected
                           averageDegree: Int, // parameter for small world network: should be even
                           nonLatticeProbability: Double, // parameter for small world network generation (beta parameter)
                           // Other model parameters
                           numberOfActions: Int,
                           minActionReward: Double, // Individual reward for least preferred action of an agent
                           absoluteCoordinationCost: Boolean, // true = non-matching actions produce utility 0, false = decreasing utility as more different
                           interactionsInstigatedPerRound: Int, // Number of interactions each agent instigates per round
                           observeNeighbours: Boolean, // False: agents only observe rewards from own interactions, true: also observe neighbour interactions
                           proposalIterations: Int, // Number of re-evaluations of best strategy producing the same strategy before proposing it as mutual commitment
                           explorationProbability: Double, // Likelihood of an agent trying a random action for a round
                           mutualCommit: Boolean, // Is mutual commitment turned on?
                           // Simulation parameters
                           numberOfRounds: Int,
                           numberOfSimulations: Int,
                           loggingLevel: LoggingLevel
                         ) {
  val extremeLog: Boolean = loggingLevel == ExtremeLogging
  val simulationLog: Boolean = loggingLevel == SimulationLogging || extremeLog
  val aggregateLog: Boolean = loggingLevel == AggregateLogging || simulationLog

  def logIfExtreme[A] (logger: A => String)(a: A): A = {
    if (extremeLog) println (logger (a))
    a
  }

  def measureAndLogIfExtreme[A] (measureName: String, logger: A => String)(a: A): A = {
    Measure.startMeasure (measureName)
    val result = logIfExtreme (logger)(a)
    Measure.endMeasure (measureName)
    result
  }

  override def toString: String =
    s"${if (mutualCommit) "mutual commit" else "no mutual commit"}, " +
      s"$numberOfAgents agents, $numberOfActions actions, $networkType, $numberOfRounds rounds, $minActionReward min action reward, " +
      s"${if (absoluteCoordinationCost) "absolute coordination" else "weighted coordination"}, " +
      s"$interactionsInstigatedPerRound interactions per round, " +
      s"${explorationProbability * 100}% exploration probability, " +
      s"${if (mutualCommit) s"propose each $proposalIterations iterations, "}" +
      s"$numberOfSimulations simulations"
}

/**
  * 100% convergence:
  * 10 agents, 3 interactions, 0.1 exploration, 1 copy, fully connected, 2 actions: 70 rounds
  * 20 agents, 1 interaction, 0.1 exploration, 1 copy, fully connected, 2 actions: 150 rounds
  * 40 agents, 1 interaction, 0.1 exploration, 1 copy, fully connected, 2 actions: 250 rounds
  * 80 agents, 1 interaction 0.05 exploration, 1 copy, fully connected, 2 actions: 150 rounds
  * 200 agents, 1 interaction, 0.05 exploration, 1 copy, fully connected, 2 actions: 110 rounds
  * 200 agents, 1 interaction, 0.05 exploration, 1 copy, fully connected, 3 actions: 275 rounds
  * 200 agents, 1 interaction, 0.01 exploration, 1 copy, fully connected, 3 actions: 70 rounds
  * 200 agents, 1 interaction, 0.01 exploration, 1 copy, fully connected, 10 actions: 90 rounds
  * 200 agents, 1 interaction, 0.01 exploration, 2 copy, fully connected, 10 actions: 100 rounds
  * 200 agents, 1 interaction, 0.01 exploration, 1 copy, fully connected, 100 actions: 110 rounds
  * 200 agents, 1 interaction, 0.01 exploration, 1 copy, small world, 100 actions: 280 rounds
  * 200 agents, 1 interaction, 0.01 exploration, 1 copy, scale-free, 100 actions: not even 1000 rounds
  */

// Configuration for real evaluation runs
object ConfigurationA extends Configuration (
  numberOfAgents = 90,
  networkType = ScaleFreeNetwork,
  averageDegree = 4,
  nonLatticeProbability = 0.4,
  numberOfActions = 100,
  minActionReward = 0.1,
  interactionsInstigatedPerRound = 1,
  explorationProbability = 0.01,
  observeNeighbours = true,
  absoluteCoordinationCost = true,
  mutualCommit = false,
  numberOfRounds = 200,
  numberOfSimulations = 1, //200,
  proposalIterations = 5,
  loggingLevel = NoLogging
)

/**
  * TAAS evaluation to try:
  *  - Can observe neighbours interactions with others
  *  - Fixed strategy agents
  *  - Larger distinction between same and different in graded corordination
  *  - Add plotting
  *  - Additive coordination rather than multiplicative (utility from being social)
  *  - Negative costs for miscoordination
  */

// Configuration for debugging runs on large network with logging at aggregate level
object ConfigurationB extends Configuration (
  numberOfAgents = 200,
  networkType = SmallWorldNetwork,
  averageDegree = 4,
  nonLatticeProbability = 0.4,
  numberOfActions = 2,
  minActionReward = 0.5,
  interactionsInstigatedPerRound = 4,
  explorationProbability = 0.1,
  observeNeighbours = false,
  absoluteCoordinationCost = true,
  mutualCommit = false,
  numberOfRounds = 10000,
  numberOfSimulations = 1,
  proposalIterations = 5,
  loggingLevel = SimulationLogging
)

// Configuration for debugging runs on very small network with logging at fine-grained level
object ConfigurationC extends Configuration (
  numberOfAgents = 10,
  networkType = ScaleFreeNetwork,
  averageDegree = 4,
  nonLatticeProbability = 0.4,
  numberOfActions = 100,
  minActionReward = 0.1,
  interactionsInstigatedPerRound = 2,
  explorationProbability = 0.01,
  observeNeighbours = false,
  absoluteCoordinationCost = true,
  mutualCommit = false,
  numberOfRounds = 10,
  numberOfSimulations = 1,
  proposalIterations = 5,
  loggingLevel = ExtremeLogging
)
