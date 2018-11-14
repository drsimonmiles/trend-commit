import Analysis._
import Experiment._
import Utilities._

object Main extends App {
  import ConfigurationA.{numberOfRounds, numberOfSimulations}

  val results = generateSeries (Vector (ConfigurationA))
  val result = results.values.head
  val converging = firstConverge (result)

  // Print percentage of simulations that converged to a norm
  println (s"Simulations converging to a norm: ${result.size.toDouble / numberOfSimulations * 100}%")
//  println (s"Simulations converging to a norm (no committing): ${convergingNoCommitting.size.toDouble / numberOfSimulations * 100}%")
//  println (s"Simulations converging to a norm (committing): ${convergingCommitting.size.toDouble / numberOfSimulations * 100}%")

  // Print round that simulations first converged to norm, on average
  println (s"Average round first converged: ${mean (converging.map (_._2.toDouble)).toInt} of $numberOfRounds")
//  println (s"Average round first converged (no committing): ${mean (convergingNoCommitting.map (_._2.toDouble)).toInt} of $numberOfRounds")
//  println (s"Average round first converged (committing): ${mean (convergingCommitting.map (_._2.toDouble)).toInt} of $numberOfRounds")

  // Print cumulative utility for whole simulation duration, averaged over all simulations
  println (s"Total cumulative utility: ${mean (result.map (result => cumulativeUtility (result, 0, numberOfRounds)))}")
//  println (s"Total cumulative utility (no committing): ${mean (recordNoCommitting.map (result => cumulativeUtility (result, 0, numberOfRounds)))}")
//  println (s"Total cumulative utility (committing): ${mean (recordCommitting.map (result => cumulativeUtility (result, 0, numberOfRounds)))}")

  // Print cumulative utility for whole simulation duration prior to convergence, averaged over all converging simulations
  println (s"Cumulative utility prior to convergence: ${mean (converging.map (result => cumulativeUtility (result._1, 0, result._2)))}")
//  println (s"Cumulative utility prior to convergence (no committing): ${mean (convergingNoCommitting.map (result => cumulativeUtility (result._1, 0, result._2)))}")
//  println (s"Cumulative utility prior to convergence (committing): ${mean (convergingCommitting.map (result => cumulativeUtility (result._1, 0, result._2)))}")

  // Print cumulative utility for whole simulation duration after convergence, averaged over all converging simulations
  println (s"Cumulative utility after convergence: ${mean (converging.map (result => cumulativeUtility (result._1, result._2, numberOfRounds)))}")
//  println (s"Cumulative utility after convergence (no committing): ${mean (convergingNoCommitting.map (result => cumulativeUtility (result._1, result._2, numberOfRounds)))}")
//  println (s"Cumulative utility after convergence (committing): ${mean (convergingCommitting.map (result => cumulativeUtility (result._1, result._2, numberOfRounds)))}")

}
