object Analysis {
  val convergencePercentage = 0.95

  // Returns true if any strategy has converged given the list of strategies agents have in a round
  def hasConverged (strategies: Vector[Action]): Boolean =
    strategies.distinct.exists (
      norm => (strategies.count (_ == norm).toDouble / strategies.size) > convergencePercentage
    )

  // Returns the round in the given results where the population first converges to a norm, or -1 if no convergence
  def firstConverge (results: SimulationRecord): Int =
    results.strategies.indexWhere (hasConverged)

  // Generate a map of simulation records to the round where they first converged, excluding simulations that did not converge
  def firstConverge (records: Vector[SimulationRecord]): Vector[(SimulationRecord, Int)] =
    records.map (sim => (sim, firstConverge (sim))).filter (_._2 != -1)

  // Calculate the cumulative utility over the given rounds (from <= round < until) of a simulation
  def cumulativeUtility (results: SimulationRecord, from: Int, until: Int): Double =
    results.roundUtility.slice (from, until).sum
}
