package org.deeplearning4j.scalphagozero.board

object NeighborTables {

  private var neighborTables: Map[Int, NeighborMap] = Map()
  private var diagonalTables: Map[Int, NeighborMap] = Map()

  /**
    * @param size size of the board to create map for
    * @return a map from a point on the board to its (up to) 4 neighbors, for specified board size.
    */
  def getNbrTable(size: Int): NeighborMap = {
    if (!neighborTables.contains(size)) {
      initNeighborTable(size)
    }
    neighborTables(size)
  }

  def getDiagnonalTable(size: Int): NeighborMap = {
    if (!diagonalTables.contains(size)) {
      initDiagonalTable(size)
    }
    diagonalTables(size)
  }

  private def initNeighborTable(size: Int): Unit = {
    var neighborMap = new NeighborMap()
    for (r <- 1 to size; c <- 1 to size) {
      val point = Point(r, c)
      val allNeighbors = point.neighbors
      val trueNeighbors = inRange(size, allNeighbors)
      neighborMap += (Point(r, c) -> trueNeighbors)
    }
    neighborTables += (size -> neighborMap)
  }

  /** For each point in the grid, the map has the diagonals from that point */
  private def initDiagonalTable(size: Int): Unit = {
    var diagonalMap = new NeighborMap()
    for (r <- 1 to size; c <- 1 to size) {
      val point = Point(r, c)
      val allDiagonals = point.diagonals
      val trueDiagonals = inRange(size, allDiagonals)
      diagonalMap += (Point(r, c) -> trueDiagonals)
    }
    diagonalTables += (size -> diagonalMap)
  }

  private def inRange(size: Int, points: List[Point]): List[Point] =
    for (nb <- points if 1 <= nb.row && nb.row <= size && 1 <= nb.col && nb.col <= size) yield nb
}
