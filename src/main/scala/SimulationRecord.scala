import FixedConfiguration._
import Networks._
import Utilities._

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
case class SimulationRecord (initialStrategies: Vector[Action], actionRewards: Map[Agent, Map[Action, Double]],
                             strategies: Vector[Vector[Action]],
                             roundUtility: Vector[Double], interactions: Option[Vector[Vector[InteractionRecord]]]) {
  // Returns the round where the population first converges to a norm, or -1 if no convergence
  def firstConvergesAt: Int = {
    // Returns true if any strategy has converged given the list of strategies agents have in a round
    def hasConverged (roundStrategies: Vector[Action]): Boolean =
      roundStrategies.distinct.exists (norm => (roundStrategies.count (_ == norm).toDouble / roundStrategies.size) > convergencePercentage)
    strategies.indexWhere (hasConverged)
  }
  // Returns the strategy held by the largest number of agents in a given round
  def mostPrevalentStrategy (round: Int): Action =
    strategies (round).groupBy (action => action).mapValues (_.size).maxBy (_._2)._1
  // Returns the sum of rewards of agents for the given action
  def populationActionReward (action: Action): Double =
    actionRewards.values.map (_ (action)).sum

  // Calculate the cumulative utility over the given rounds (from <= round < until) of a simulation. If from is None,
  // start from the first round. If until is None, end at the last simulation round.
  def cumulativeUtility (from: Option[Int], until: Option[Int]): Double = (from, until) match {
    case (None, None) => roundUtility.sum
    case (None, Some (u)) => roundUtility.take (u).sum
    case (Some (f), None) => roundUtility.drop (f).sum
    case (Some (f), Some (u)) => roundUtility.slice (f, u).sum
  }
  // Calculate the utility the population would get per round if all adopted the strategy converged to, or None if it doesn't converge
  def stablePopulationUtility: Option[Double] =
    if (firstConvergesAt == -1) None
    else Some (populationActionReward (mostPrevalentStrategy (firstConvergesAt)))
}

// The aggregated record of multiple simulation runs of the same configuration, with statistics on how these performed on average
class AggregateRecord (val configuration: Configuration, val runs: Vector[SimulationRecord]) {
  // Generate a map of simulation records to the round where they first converged, excluding simulations that did not converge
  val firstConvergesAt: Map[SimulationRecord, Int] = runs.createMap (_.firstConvergesAt).filter (_._2 != -1)
  val proportionConverging: Double = firstConvergesAt.size.toDouble / runs.size
  val averageConvergeRound: Int = mean (firstConvergesAt.toVector.map (_._2.toDouble)).toInt
  val averageTotalUtility: Double = mean (runs.map (_.cumulativeUtility (None, None)))
  val averageUtilityBeforeConvergence: Double = mean (firstConvergesAt.toVector.map (result => result._1.cumulativeUtility (None, Some (result._2))))
  val averageUtilityAfterConvergence: Double = mean (firstConvergesAt.toVector.map (result => result._1.cumulativeUtility (Some (result._2), None)))
  val averageStablePopulationUtility: Double = mean (runs.map (_.stablePopulationUtility).collect {case Some (u) => u})

  def averageUtilityUntilRound (round: Int): Double =
    mean (runs.map (_.cumulativeUtility (None, Some (round))))
  def averageUtilityFromRound (round: Int): Double =
    mean (runs.map (_.cumulativeUtility (Some (round), None)))

  override def toString: String = runs.map (_.firstConvergesAt).toString +
    s"Simulations converging to a norm: ${proportionConverging * 100}%\n" +
    s"Average round first converged: $averageConvergeRound\n" +
    s"Total cumulative utility: $averageTotalUtility\n" +
    s"Cumulative utility prior to convergence: $averageUtilityBeforeConvergence\n" +
    s"Cumulative utility after convergence: $averageUtilityAfterConvergence\n"
}

// Combination of two aggregated sets of results under different configuration, with statistics comparing the two
class ComparativeRecord (val condition1: AggregateRecord, val condition2: AggregateRecord) {
  val utilityLostUpToConvergence: Double =
    condition2.averageUtilityUntilRound (condition1.averageConvergeRound) - condition1.averageUtilityBeforeConvergence
  val stableUtilityGained: Double =
    condition1.averageStablePopulationUtility - condition2.averageStablePopulationUtility
}
