import scala.util.Random.{nextDouble => randomDouble, nextInt => randomInt}

/**
  * Algorithms to generate networks, and utility functions to query them.
  * Networks are structured as a set of (Int, Int) tuples representing each undirected edge between two
  * nodes. The nodes are integers from 0 to numberOfNodes.
  */
object Networks {
  case class Agent (id: Int) extends AnyVal {
    override def toString: String = s"Ag$id"
  }

  /** Generate a scale-free network with a given number of nodes using Barabási-Albert model. */
  def scaleFreeNetwork (numberOfNodes: Int): Vector[(Agent, Agent)] = {
    // Given a set of edges created so far and the degrees of nodes added so far, add the rest of the nodes
    def addNodes (edgesSoFar: Vector[(Agent, Agent)], nodeDegrees: Vector[Int], degreeSum: Int): Vector[(Agent, Agent)] =
      // If the number of nodes we've added is the total, return the network
      if (nodeDegrees.size == numberOfNodes) edgesSoFar
      else {
        // Sum the degrees of all nodes, choose a random value from 1 to that sum
        // Treat this value like an index to find an existing node which the new node is then connected to
        // The index locates the node by deducting each node degree until the index reduces to/below zero
        // This is equivalent to selecting a node with a probability based on its node degree
        def locateNode (position: Int, fromIndex: Int): Int =
          if (position - nodeDegrees (fromIndex) <= 0) fromIndex
          else locateNode (position - nodeDegrees (fromIndex), fromIndex + 1)
        val connectTo = locateNode (randomInt (degreeSum) + 1, 0)
        // Update the node degrees to account for the new edge and add the remaining nodes
        val newDegrees = 1 +: nodeDegrees.updated (connectTo, nodeDegrees (connectTo) + 1)
        addNodes ((Agent (nodeDegrees.size), Agent (connectTo)) +: edgesSoFar, newDegrees, degreeSum + 2)
      }
    // Start with the first two nodes and their connecting edge in place and then add the rest
    addNodes (Vector ((Agent (0), Agent (1))), Vector (1, 1), 2)
  }

  /** Generate a small world network with a given number of nodes, average degree and
    * non-lattice probability (beta parameter) using Watts-Strogatz model */
  def smallWorldNetwork (numberOfNodes: Int, averageDegree: Int, nonLatticeProbability: Double): Vector[(Agent, Agent)] = {
    // Generate a regular ring lattice as a starting point
    val lattice = ringLattice (numberOfNodes, averageDegree)
    // Convenience function to return a value unchanged if below a threshold, value+1 if over the threshold
    def incrementIfOver (value: Int, threshold: Int): Int = if (value < threshold) value else value + 1
    // Returns a random node excluding the given node
    def randomNodeExcluding (exclude: Int): Int = incrementIfOver (randomInt (numberOfNodes - 1), exclude)
    // Returns true if the neighbour is within averageDegree/2 to the right of node around the ring
    def withinDistanceToRight (neighbour: Int, node: Int): Boolean =
      (if (neighbour > node) neighbour else neighbour + numberOfNodes) <= node + averageDegree / 2

    // For each node, replace each edge it has to a node within averageDistance/2 around the ring, with
    // a probability of nonLatticeProbability, with an edge to a random other node
    (for (node <- 0 until numberOfNodes; neighbour <- connected (Agent (node), lattice)) yield {
      if (withinDistanceToRight (neighbour.id, node) && randomDouble () < nonLatticeProbability)
        (Agent (node), Agent (randomNodeExcluding (node)))
      else
        (Agent (node), neighbour)
    }).toVector
  }

  /** Generate a regular ring lattice with a given number of nodes each with a given number of neighbours.
    * The node IDs will go around the ring in continguous numerical order. */
  def ringLattice (numberOfNodes: Int, numberOfNeighbours: Int): Vector[(Agent, Agent)] =
    (for (nodeA <- 0 until numberOfNodes; nodeB <- 0 until numberOfNodes) yield {
      val order = (nodeA - nodeB).abs % (numberOfNodes - 1 - (numberOfNeighbours / 2))
      if (order > 0 && order <= numberOfNeighbours / 2) Vector ((Agent (nodeA), Agent (nodeB)))
      else Vector.empty
    }).flatten.toVector

  def fullyConnectedNetwork (numberOfNodes: Int): Vector[(Agent, Agent)] =
    (for (nodeA <- 0 until numberOfNodes - 1; nodeB <- nodeA + 1 until numberOfNodes) yield {
      (Agent (nodeA), Agent (nodeB))
    }).toVector

  /** Return the nodes in a (sub-)network for which the edges between nodes are given */
  def nodes (network: Vector[(Agent, Agent)]): Vector[Agent] =
    network.flatMap (edge => Vector (edge._1, edge._2)).distinct

  /** Return the nodes connected to the given node from the given network */
  def connected (node: Agent, network: Vector[(Agent, Agent)]): Vector[Agent] =
    nodes (network.filter (edge => edge._1 == node || edge._2 == node)).filter (_ != node)
}
