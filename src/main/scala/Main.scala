import co.theasi.plotly._
import Analysis._
import Experiment._
import Utilities._

object Main extends App {
  import ConfigurationA.{numberOfRounds, numberOfSimulations}

  val results = generateSeries (Vector (ConfigurationA))
  val result = results.values.head
  val converging = firstConverge (result)

  println (s"Simulations converging to a norm: ${result.size.toDouble / numberOfSimulations * 100}%")
  println (s"Average round first converged: ${mean (converging.map (_._2.toDouble)).toInt} of $numberOfRounds")
  println (s"Total cumulative utility: ${mean (result.map (result => cumulativeUtility (result, 0, numberOfRounds)))}")
  println (s"Cumulative utility prior to convergence: ${mean (converging.map (result => cumulativeUtility (result._1, 0, result._2)))}")
  println (s"Cumulative utility after convergence: ${mean (converging.map (result => cumulativeUtility (result._1, result._2, numberOfRounds)))}")

  val series = configSequence (ConfigurationA,
    c => c.copy (interactionsInstigatedPerRound = c.interactionsInstigatedPerRound + 1),
    _.interactionsInstigatedPerRound == 5)
  val newResults = generateSeries (series)
}
