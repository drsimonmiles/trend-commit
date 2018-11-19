import Utilities.mean
import co.theasi.plotly.{Plot, draw}

object Output {
  def printTable (headings: Vector[String], data: Vector[Vector[Any]]): Unit = {
    val texts: Vector[Vector[String]] = data.map (_.map (_.toString))
    val headedData: Vector[Vector[String]] = headings.zip (texts).map (c => c._1 +: c._2)
    val widths: Vector[Int] = headedData.map (_.map (_.length).max + 1)
    val sized: Vector[Vector[String]] = widths.zip (headedData).map (c => c._2.map (_.padTo (c._1, ' ')))
    val numberOfRows: Int = sized.head.size
    val rows: Vector[String] = (0 until numberOfRows).map (i => sized.map (_ (i)).mkString).toVector

    rows.foreach (println)
  }

  def showTable (xHeading: String, yHeading: String, data: Vector[(Any, Any)]): String = {
    val texts: Vector[Vector[String]] = Vector (data.map (_._1.toString), data.map (_._2.toString))
    val headedData: Vector[Vector[String]] = Vector (xHeading, yHeading).zip (texts).map (c => c._1 +: c._2)
    val widths: Vector[Int] = headedData.map (_.map (_.length).max + 1)
    val sized: Vector[Vector[String]] = widths.zip (headedData).map (c => c._2.map (_.padTo (c._1, ' ')))
    val numberOfRows: Int = sized.head.size
    (0 until numberOfRows).map (i => sized.map (_ (i)).mkString).mkString ("\n")
  }

  def showMultiTable[SeriesValue] (tableHeadings: SeriesValue => String, xHeading: String, yHeading: String,
                                   data: Vector[(SeriesValue, Vector[(Any, Any)])]): String =
    data.map (d => tableHeadings (d._1) + "\n" + showTable (xHeading, yHeading, d._2)).mkString ("\n\n")

  def plotConvergence (records: Vector[SimulationRecord]): Unit = {
    val config = records.head.configuration
    val xs = 0 until config.numberOfRounds
    val ys = xs.map (round =>
      mean (records.map (_.strategies (round).count (_ == Action (0)).toDouble / config.numberOfAgents)))
    val plot = Plot ().withScatter (xs, ys)

    draw (plot, s"Ac0 agents (${config.networkType} ${config.copyFrequency}-${config.absoluteCoordinationCost})")
  }

}
