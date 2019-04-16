import java.io.{FileWriter, PrintWriter}

import Experiment._
import Output._
import Simulation.simulate

object Main extends App {
  //simulate (ConfigurationA)
  //println (Measure.toString)

  def logLine (filename: String, line: String): Unit = {
    val logger = new PrintWriter (new FileWriter (filename, true))
    logger.println (line)
    logger.close ()
  }

  val log = "convergence.txt"
  val startingPopulation = 10
  val endingPopulation = 20000
  val startingRounds = 200
  val populationIncrement = 10
  val roundsIncrement = 100

  def simulateToConvergence (config: Configuration): Configuration = {
    logLine (log, s"Trying ${config.numberOfRounds} rounds for ${config.numberOfAgents} agents")
    if (simulate (config).proportionConverging < 0.5)
      simulateToConvergence (config.copy (numberOfRounds = config.numberOfRounds + roundsIncrement))
    else config
  }

  (startingPopulation to endingPopulation by populationIncrement).foldLeft (ConfigurationA.copy (numberOfRounds = startingRounds): Configuration) {
    (config, population) =>
      val found = simulateToConvergence (config.copy (numberOfAgents = population))
      logLine (log, "Success")
      found
  }

  // Standards case: Cost of changing strategy; copy based on how well currently doing (from wider observation?); no exploration?
  // Innovation case: Initial action space removes a lot of actions; exploration allows some better to appear

  /**
    * Two measures to compare with and without mutual commit:
    *  - Utility lost from not committing: in time up to convergence in non-commit case, difference in cumulative utility
    *  - Quality of stable state: Difference in per-round population-aggregate utility of emergent strategy
    */
 /*val multiSeries = configMultiSequence (ConfigurationA.copy (proposalIterations = 1),
    c => c.copy (mutualCommit = true), _.mutualCommit == true,
    c => c.copy (proposalIterations = c.proposalIterations + 1), _.proposalIterations == 10)
  val newResults: Vector[Vector[AggregateRecord]] =
    generateMultiSeries (multiSeries)
  println (newResults.flatten.map (_.averageTotalUtility).sum)
  val comparison: Vector[ComparativeRecord] =
    compareSeries (newResults (0), newResults (1))
  println (showComparisons (comparison, "Iterations for proposal", _.proposalIterations))*/

}
