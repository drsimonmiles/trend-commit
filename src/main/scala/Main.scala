import Experiment._
import Output._

object Main extends App {
/*  val results = generateSeries (Vector (ConfigurationA))
  val result: AggregateRecord = results.values.head

  println (result)*/

  /**
    * Two measures to compare with and without mutual commit:
    *  - Utility lost from not committing: in time up to convergence in non-commit case, difference in cumulative utility
    *  - Quality of stable state: Difference in per-round population-aggregate utility of emergent strategy
    */

//  val series = configSequence (ConfigurationA,
//    c => c.copy (interactionsInstigatedPerRound = c.interactionsInstigatedPerRound + 1),
//    _.interactionsInstigatedPerRound == 5)
//  val newResults = generateSeries (series)
//  val xs = series.map (_.interactionsInstigatedPerRound)
//  val ys = series.map (config => newResults (config).averageConvergeRound)
//  printTable (Vector ("Interactions instigated", "Round converged"), Vector (xs, ys))

  val multiSeries = configMultiSequence (ConfigurationA,
    c => c.copy (mutualCommit = true), _.mutualCommit == true,
    c => c.copy (explorationProbability = c.explorationProbability + 0.005), _.explorationProbability >= 0.05)
  val newResults: Vector[Map[Configuration, AggregateRecord]] =
    generateMultiSeries (multiSeries)
  val data: Vector[(Boolean, Vector[(Double, Int)])] =
    multiSeriesToData (multiSeries, newResults, _.mutualCommit, _.explorationProbability, _.averageConvergeRound)
  println (showMultiTable[Boolean] (c => s"Mutual commit: $c", "Exploration", "Round converged", data))

}
