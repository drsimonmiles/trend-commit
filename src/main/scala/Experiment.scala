import Simulation.simulate
import Utilities._

object Experiment {
  def configSequence (zero: Configuration, next: Configuration => Configuration, stopAt: Configuration => Boolean): Vector[Configuration] =
    if (stopAt (zero)) Vector (zero) else zero +: configSequence (next (zero), next, stopAt)

  def configMultiSequence (zero: Configuration, nextSeries: Configuration => Configuration, stopAtSeries: Configuration => Boolean,
                           nextInSeries: Configuration => Configuration, stopAtInSeries: Configuration => Boolean): Vector[Vector[Configuration]] =
    if (stopAtSeries (zero)) Vector (configSequence (zero, nextInSeries, stopAtInSeries))
    else configSequence (zero, nextInSeries, stopAtInSeries) +:
      configMultiSequence (nextSeries (zero), nextSeries, stopAtSeries, nextInSeries, stopAtInSeries)

  def generateSeries (configurations: Vector[Configuration]): Map[Configuration, AggregateRecord] =
    configurations.createMap (simulate)

  def generateMultiSeries (configurations: Vector[Vector[Configuration]]): Vector[Map[Configuration, AggregateRecord]] =
    configurations.map (_.createMap (simulate))

  def multiSeriesToData[SeriesValue, XValue, YValue] (configs: Vector[Vector[Configuration]],
                                                      results: Vector[Map[Configuration, AggregateRecord]],
                                                      seriesValue: Configuration => SeriesValue,
                                                      xValue: Configuration => XValue,
                                                      yValue: AggregateRecord => YValue): Vector[(SeriesValue, Vector[(XValue, YValue)])] =
    configs.zip (results).map (z => (seriesValue (z._1.head), z._1.map (c => (xValue (c), yValue (z._2 (c))))))
}
