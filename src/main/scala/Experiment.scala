import Simulation.simulate
import Utilities._

object Experiment {
  def configSequence (zero: Configuration, next: Configuration => Configuration, stopAt: Configuration => Boolean): Vector[Configuration] =
    if (stopAt (zero)) Vector (zero) else zero +: configSequence (next (zero), next, stopAt)

  def generateSeries (configurations: Vector[Configuration]): Map[Configuration, Vector[SimulationRecord]] =
    configurations.createMap (simulate)

  def generateMultiseries (configurations: Vector[Vector[Configuration]]): Vector[Map[Configuration, Vector[SimulationRecord]]] =
    configurations.map (_.createMap (simulate))


}
