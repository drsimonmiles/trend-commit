import scala.util.Random.{nextInt => randomInt}

object Utilities {
  // Calculate the mean average of a list of values
  def mean (values: Vector[Double]): Double =
    values.sum / values.size

  // Choose a random value from a vector
  def randomChoice[A] (collection: Vector[A]): A =
    collection (randomInt (collection.size))

  implicit class CollectionExtensions[A] (vector: Iterable[A]) {
    def createMap[B] (f: A => B): Map[A, B] =
      vector.map (e => (e, f (e))).toMap
  }

  implicit class MapExtensions[A, B] (map: Map[A, B]) {
    // Map the values of this map to create a new map, using a function over both the key and value
    def transformValues[C] (f: (A, B) => C): Map[A, C] =
      map.keys.map (k => (k, f (k, map (k)))).toMap
  }
}
