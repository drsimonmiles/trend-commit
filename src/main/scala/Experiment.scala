import Simulation.simulate
import Utilities._

object Experiment {
  def generateSeries (configurations: Vector[Configuration]): Map[Configuration, Vector[SimulationRecord]] =
    configurations.createMap (simulate)

  def generateMultiseries (configurations: Vector[Vector[Configuration]]): Vector[Map[Configuration, Vector[SimulationRecord]]] =
    configurations.map (_.createMap (simulate))
}
