import Experiment._
import Output._

object Main extends App {
  /**
    * Two measures to compare with and without mutual commit:
    *  - Utility lost from not committing: in time up to convergence in non-commit case, difference in cumulative utility
    *  - Quality of stable state: Difference in per-round population-aggregate utility of emergent strategy
    */
  val multiSeries = configMultiSequence (ConfigurationA.copy (proposalIterations = 1),
    c => c.copy (mutualCommit = true), _.mutualCommit == true,
    c => c.copy (proposalIterations = c.proposalIterations + 1), _.proposalIterations == 5)
  val newResults: Vector[Vector[AggregateRecord]] =
    generateMultiSeries (multiSeries)
  val comparison: Vector[ComparativeRecord] =
    compareSeries (newResults (0), newResults (1))
  println (showComparisons (comparison, "Iterations for proposal", _.proposalIterations))

}
