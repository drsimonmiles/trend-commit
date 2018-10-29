import co.theasi.plotly._
import Networks._
import Simulation._

object Logging {
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

  def strategyCounts (strategies: Vector[Action]): Vector[(Action, Int)] =
    strategies.groupBy (action => action).mapValues (_.size).toVector

  def topStrategy (strategies: Vector[Action]): Action =
    strategyCounts (strategies).maxBy (_._2)._1

  def topStrategyInRound (record: SimulationRecord, round: Int): Action =
    topStrategy (record.strategies (round))

  def switchersInRound (record: SimulationRecord, round: Int): Vector[(Action, Int)] = {
    val prior = strategyCounts (if (round == 0) record.initialStrategies else record.strategies (round - 1)).toMap
    strategyCounts (record.strategies (round)).map (count => (count._1, count._2 - prior.getOrElse (count._1, 0)))
  }

  def logSwitchersInRound (records: Vector[SimulationRecord], round: Int): String =
    records.flatMap (switchersInRound (_, round)).groupBy (_._1).mapValues (v => mean (v.map (_._2.toDouble))).toVector.
      sortBy (_._2).reverse.map (s => s._1 + s": ${if (s._2 > 0) "+" + s._2 else s._2}").mkString (", ")

  def logTopStrategiesInRoundsRange (records: Vector[SimulationRecord], fromRound: Int, toRound: Int): String =
    (for (round <- fromRound to toRound) yield
      records.map (topStrategyInRound (_, round)).toSet.mkString (s"$round: {", ",", "}") +
        "  " + logSwitchersInRound (records, round)
        ).mkString ("\n")

  def logTopStrategiesPerRound (records: Vector[SimulationRecord], openingRounds: Int, closingRounds: Int, numberOfRounds: Int): String =
    "Most prevalent strategies per round:\n" +
      records.map (record => topStrategy (record.initialStrategies)).toSet.mkString ("i: {", ",", "}") + "\n" +
      logTopStrategiesInRoundsRange (records, 0, openingRounds - 1) +
      "\n ... \n" +
      logTopStrategiesInRoundsRange (records, numberOfRounds - closingRounds, numberOfRounds - 1)

  def logFinalStrategyDistribution (records: Vector[SimulationRecord]): String =
    "Final strategy distribution per simulation:\n" +
    records.map (r => strategyCounts (r.strategies.last).map (s => s"${s._1}: ${s._2}").mkString ("{", ", ", "}")).mkString ("\n")

  def logObservedStrategyUtilitiesPerRound (records: Vector[SimulationRecord], openingRounds: Int, closingRounds: Int, numberOfRounds: Int, agents: Vector[Agent]): String = {
    def observedUtilitiesInRound (record: SimulationRecord, round: Int): Vector[Map[Action, Vector[Double]]] =
      record.interactions match {
        case None => Vector.empty
        case Some (allInteractions) =>
          agents.map (agent => actionRewards (observedInteractions (agent, allInteractions (round))))
      }

    def bestStrategiesInRound (record: SimulationRecord, round: Int): Vector[Action] =
      record.interactions match {
        case None => Vector.empty
        case Some (allInteractions) =>
          agents.map (agent => bestStrategy (observedInteractions (agent, allInteractions (round))))
      }

    def mergeObservedStrategyUtilities (utilities: Vector[Map[Action, Vector[Double]]]): Map[Action, Double] = {
      def mergeTwo (utilities1: Map[Action, Vector[Double]], utilities2: Map[Action, Vector[Double]]): Map[Action, Vector[Double]] =
        (utilities1.keySet ++ utilities2.keySet).map (action =>
          (action, utilities1.getOrElse (action, Vector.empty) ++ utilities2.getOrElse (action, Vector.empty))
        ).toMap
      utilities.reduce (mergeTwo).mapValues (mean)
    }

    def percentages (strategies: Vector[Action]): Map[Action, Double] = {
      val counts = strategies.distinct.map (action => (action, strategies.count (_ == action)))
      val total = counts.map (_._2).sum
      counts.toMap.mapValues (_.toDouble / total)
    }

    def logObservedStrategyUtilitiesInRoundRange (records: Vector[SimulationRecord], fromRound: Int, toRound: Int): String =
      (for (round <- fromRound to toRound) yield
        mergeObservedStrategyUtilities (records.flatMap (observedUtilitiesInRound (_, round))).toVector.sortBy (_._2).reverse.
          map (s => f"${s._1}: ${s._2}%.3f").mkString (s"$round util: {", ", ", "}\n") +
        percentages (records.flatMap (bestStrategiesInRound (_, round))).toVector.sortBy (_._2).reverse.
          map (s => f"${s._1}: ${s._2}%.1f%%").mkString (s"  best: {", ", ", "}")
        ).mkString ("\n")

    "Utility observed per strategy per round: \n" +
      logObservedStrategyUtilitiesInRoundRange (records, 0, openingRounds - 1) +
      "\n...\n" +
      logObservedStrategyUtilitiesInRoundRange (records, numberOfRounds - closingRounds, numberOfRounds - 1)
  }

  def plotConvergence (record: SimulationRecord): Unit = {
    val xs = record.strategies.indices
    val ys = record.strategies.map (_.count (_ == Action (0)).toDouble / record.configuration.numberOfAgents)
    val plot = Plot ().withScatter (xs, ys)

    draw (plot, s"Proportion of agents using strategy 0 (${record.configuration.networkType})")
  }
}
