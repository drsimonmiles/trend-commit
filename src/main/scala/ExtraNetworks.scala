import Networks.{connected, nodes}
import scala.util.Random.{nextInt => randomInt}

object ExtraNetworks {
  /** Remove an edge from a network regardless of which direction, i.e. (A, B) or (B, A), it is stored in */
  def removeEdge (nodeA: Int, nodeB: Int, network: Vector[(Int, Int)]): Vector[(Int, Int)] =
    network.filter (edge => (edge._1 != nodeA || edge._2 != nodeB) && (edge._1 != nodeB || edge._2 != nodeA))

  /** Returns whether an edge is in a network regardless of which direction, i.e. (A, B) or (B, A), it is stored in */
  def containsEdge (nodeA: Int, nodeB: Int, network: Vector[(Int, Int)]): Boolean =
    network.contains ((nodeA, nodeB)) || network.contains ((nodeB, nodeA))

  /** Calculate the mean average degree of nodes in the network */
  def averageDegree (network: Vector[(Int, Int)]): Double = {
    val degreePerNode: Vector[Int] =
      nodes (network).map (node => connected (node, network).size)
    degreePerNode.sum.toDouble / degreePerNode.size
  }

  /** Generate a network between a set of nodes where there is one edge between each node and every other */
  def fullyConnectedNetwork (numberOfNodes: Int): Vector[(Int, Int)] =
    (0 until numberOfNodes - 1).flatMap (from => (from + 1 until numberOfNodes).map (to => (from, to))).toVector

  /** Generate a random connected network */
  def randomConnectedNetwork (numberOfNodes: Int): Vector[(Int, Int)] = {
    // Continue random walk through nodes until all visited, return list of nodes visited in order
    def walkFrom (walkSoFar: Vector[Int]): Vector[Int] =
      if (walkSoFar.distinct.size == numberOfNodes)
        walkSoFar
      else
        walkFrom (randomInt (numberOfNodes) +: walkSoFar)
    // Do whole random walk covering whole graph, a spanning tree
    val randomWalk: Vector[Int] = walkFrom (Vector[Int] ())
    // Turn list of nodes visited into edges between them
    val edges: Vector[(Int, Int)] =
      randomWalk.sliding (2).map (step => (step.min, step.max)).toVector.distinct
    // Remove any edges from a node to itself
    edges.filter (edge => edge._1 != edge._2)
  }
}
