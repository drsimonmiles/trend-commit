import java.lang.System.currentTimeMillis

object Measure {
  val measuring: Boolean = true

  // Each block is measured by minimum time spent, maximum time spent, total time spent, number of calls to block
  private val times = scala.collection.mutable.Map[String, (Long, Long, Long, Long)] ()
  private val blockStarts = scala.collection.mutable.Map[String, Long] ()

  private def recordMeasure (name: String, start: Long): Unit = {
    val time = currentTimeMillis - start
    val prev = times.getOrElse (name, (0l, 0l, 0l, 0l))
    val min = if (time < prev._1) time else prev._1
    val max = if (time > prev._2) time else prev._2
    times (name) = (min, max, prev._3 + time, prev._4 + 1)
  }

  def measure[T] (name: String)(f: => T): T = {
    if (measuring) {
      val start = currentTimeMillis
      val result = f
      recordMeasure (name, start)
      result
    } else f
  }

  def startMeasure (name: String): Unit =
    if (measuring) blockStarts (name) = currentTimeMillis

  def endMeasure (name: String): Unit =
    if (measuring) recordMeasure (name, blockStarts (name))

  // Display each block in order: minimum - average - maximum - total - occurrences
  override def toString = {
    def tabulate (data: Vector[Vector[String]], rightJustify: Vector[Boolean]): String = {
      val columns = rightJustify.size
      val widths = (0 until columns).map (column => data.map (_ (column)).map (_.length).max)
      def justify (text: String, column: Int): String =
        if (rightJustify (column)) text.reverse.padTo (widths (column), ' ').reverse
        else text.padTo (widths (column), ' ')
      data.map (_.zipWithIndex.map (zipped => justify (zipped._1, zipped._2)).mkString (" ")).mkString ("\n")
    }

    val results = times.keys.toVector.sorted.map { key =>
      Vector (key, times (key)._1 + " (min)", (times (key)._3 / times (key)._4) + " (avg)", times (key)._2 + " (max)", times (key)._3 + " (tot)", times (key)._4 + " (count)")
    }
    tabulate (results, Vector (false, true, true, true, true, true))
  }
}
