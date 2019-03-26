import Networks.Agent
import Utilities.mean
import co.theasi.plotly.{Plot, draw}
import java.io.{File, FileWriter, PrintWriter}

object Output {
  // Save a DOT serialisation of the given network graph to the given file
  def networkToDOT (graph: Vector[(Agent, Agent)], file: File): Unit = {
    val writer = new PrintWriter (new FileWriter (file))
    val dot = graph.map (edge => s"${edge._1.id} -- ${edge._2.id};").mkString ("graph network {\n  ", "\n  ", "\n}\n")
    writer.println (dot)
    writer.close ()
  }

  // Format a matrix of data as a table with vertical padded columns, where the data is a vector of columns, each a vector of cells
  def formatTable (columns: Vector[Vector[String]]): String = {
    val widths: Vector[Int] = columns.map (_.map (_.length).max + 1)
    val sized: Vector[Vector[String]] = widths.zip (columns).map (c => c._2.map (_.padTo (c._1, ' ')))

    sized.head.indices.map (row => sized.map (_ (row)).mkString).mkString ("\n")
  }

  def printTable (headings: Vector[String], data: Vector[Vector[Any]]): Unit = {
    val texts: Vector[Vector[String]] = data.map (_.map (_.toString))
    val headedData: Vector[Vector[String]] = headings.zip (texts).map (c => c._1 +: c._2)

    print (formatTable (headedData))
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

  def showComparisons (comparisons: Vector[ComparativeRecord], variableHeading: String, variable: Configuration => Any): String = {
    val variableColumn = variableHeading +: comparisons.map (c => variable (c.condition1.configuration).toString)
    val c1 = "LHS convergence round" +: comparisons.map (_.condition1.averageConvergeRound.toString)
    val c2 = "RHS utility until that round" +: comparisons.map (c => c.condition2.averageUtilityUntilRound (c.condition1.averageConvergeRound).toString)
    val c3 = "LHS utility before convergence" +: comparisons.map (_.condition1.averageUtilityBeforeConvergence.toString)
    val lostUtilityColumn = "Pre-convergence utility loss" +: comparisons.map (_.utilityLostUpToConvergence.toString)
    val c4 = "LHS stable utility" +: comparisons.map (_.condition1.averageStablePopulationUtility.toString)
    val c5 = "RHS stable utility" +: comparisons.map (_.condition2.averageStablePopulationUtility.toString)
    val stableGainColumn = "Stable utility gain" +: comparisons.map (_.stableUtilityGained.toString)

//    formatTable (Vector (variableColumn, c1, c2, c3, lostUtilityColumn, c4, c5, stableGainColumn))
    formatTable (Vector (variableColumn, lostUtilityColumn, stableGainColumn))
  }

  def plotConvergence (records: AggregateRecord): Unit = {
    val config = records.configuration
    val xs = 0 until config.numberOfRounds
    val ys = xs.map (round =>
      mean (records.runs.map (_.strategies (round).count (_ == Action (0)).toDouble / config.numberOfAgents)))
    val plot = Plot ().withScatter (xs, ys)

    draw (plot, s"Ac0 agents (${config.networkType} ${config.absoluteCoordinationCost})")
  }

}
