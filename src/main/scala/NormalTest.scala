import scala.util.Random.{nextDouble => randomDouble, nextGaussian => randomGaussian}

object NormalTest extends App {
  val samples = 10000000
  val count = Array.fill (11)(0l)

  for (_ <- 1 to samples) {
    val mean = randomDouble
    val value = (randomGaussian / 4 + 0.5).min (1.0).max (0.0)
    count ((value * 10).toInt) += 1
  }
// val stdDev = (mean - 0.5).abs * 0.5 + 0.25
  val totalPercentage = count.sum.toDouble
  println (count.map (x => x * 10000 / totalPercentage.toInt / 100.0).mkString (" "))
}
