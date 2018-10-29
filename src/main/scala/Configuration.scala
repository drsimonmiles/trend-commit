sealed trait NetworkType
case object ScaleFreeNetwork extends NetworkType { override def toString = "scale-free" }
case object SmallWorldNetwork extends NetworkType { override def toString = "small world" }
case object FullyConnectedNetwork extends NetworkType { override def toString = "fully connected" }

case class Configuration ( // Network generation parameters
                           numberOfAgents: Int,
                           networkType: NetworkType,   // small world, scale-free, or fully connected
                           averageDegree: Int, // parameter for small world network: should be even
                           nonLatticeProbability: Double, // parameter for small world network generation (beta parameter)
                           // Other model parameters
                           numberOfActions: Int,
                           interactionsInstigatedPerRound: Int, // Number of interactions each agent instigates per round
                           explorationProbability: Double, // Likelihood of an agent trying a random action for a round
                           copyFrequency: Int, // How many rounds between agents copying each others' best strategies
                           absoluteCoordinationCost: Boolean, // true = non-matching actions produce utility 0, false = decreasing utility as more different
                           miscoordinationWeight: Double,   // CURRENTLY NOT USED
                           // Simulation parameters
                           numberOfRounds: Int,
                           numberOfSimulations: Int,
                           convergencePercentage: Double
                         )

object ConfigurationA extends Configuration (
  numberOfAgents = 200,
  networkType = ScaleFreeNetwork,
  averageDegree = 4,
  nonLatticeProbability = 0.4,
  numberOfActions = 9,
  interactionsInstigatedPerRound = 4,
  explorationProbability = 0.1,
  copyFrequency = 1,
  absoluteCoordinationCost = true,
  miscoordinationWeight = 1.0,
  numberOfRounds = 5000,
  numberOfSimulations = 10,
  convergencePercentage = 0.95
)

/**
  * TAAS evaluation to try:
  *  - Run with 2 actions, try increasing
  *  - Fully connected
  *  - Can observe neighbours interactions with others
  *  - Fixed strategy agents
  *  - Larger distinction between same and different in graded corordination
  *  - Add plotting
  */

object ConfigurationB extends Configuration (
  numberOfAgents = 200,
  networkType = SmallWorldNetwork,
  averageDegree = 4,
  nonLatticeProbability = 0.4,
  numberOfActions = 2,
  interactionsInstigatedPerRound = 4,
  explorationProbability = 0.1,
  copyFrequency = 10,
  absoluteCoordinationCost = true,
  miscoordinationWeight = 1.0,
  numberOfRounds = 10000,
  numberOfSimulations = 1,
  convergencePercentage = 0.95
)