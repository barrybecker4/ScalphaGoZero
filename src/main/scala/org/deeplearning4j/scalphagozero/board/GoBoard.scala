package org.deeplearning4j.scalphagozero.board

import ZobristHashing.ZOBRIST

/**
  * Main Go board class, represents the board on which Go moves can be played. Immutable.
  *
  * @param size the size of the go board. Values of 5, 9, 13, 17, 19, or 25 are reasonable.
  * @param grid map from grid point location to parent string of stones (if any)
  * @param hash the Zobrist hash. Gets updated as moves are played.
  * @author Max Pumperla
  * @author Barry Becker
  */
case class GoBoard(
    size: Int,
    grid: Map[Point, GoString] = Map.empty,
    hash: Long = 0L,
    blackCaptures: Int = 0,
    whiteCaptures: Int = 0
) {

  private val serializer = new GoBoardSerializer(this)

  private val neighborMap: Map[Point, List[Point]] = NeighborTables.getNbrTable(size)
  private val diagonalMap: Map[Point, List[Point]] = NeighborTables.getDiagnonalTable(size)

  def neighbors(point: Point): List[Point] = neighborMap.getOrElse(point, List.empty)

  def corners(point: Point): List[Point] = diagonalMap.getOrElse(point, List.empty)

  def placeStone(player: Player, point: Point): GoBoard = {
    assert(isOnGrid(point))

    if (getGoString(point).isDefined) {
      println(" Illegal move attempted at: " + point + ". Already occupied: " + getGoString(point).get)
      this
    } else {
      // 1. Examine adjacent points
      var adjacentSameColor = Set[GoString]()
      var adjacentOppositeColor = Set[GoString]()
      var liberties = Set[Point]()

      for (neighbor: Point <- neighborMap(point)) {
        getGoString(neighbor) match {
          case None                                        => liberties += neighbor
          case Some(goString) if goString.player == player => adjacentSameColor += goString
          case Some(goString)                              => adjacentOppositeColor += goString
        }
      }

      // 2. Merge any strings of the same color adjacent to the placed stone
      adjacentSameColor += GoString(player, Set(point), liberties)
      val newString: GoString = adjacentSameColor.reduce(_ mergedWith _)

      var newGrid = grid
      for (newStringPoint: Point <- newString.stones)
        newGrid += newStringPoint -> newString

      var newHash = hash
      newHash ^= ZOBRIST((point, None)) // Remove empty-point hash code
      newHash ^= ZOBRIST((point, Some(player))) // Add filled point hash code.

      // 3. Reduce liberties of any adjacent strings of the opposite color.
      // 4. If any opposite color strings now have zero liberties, remove them.
      var stringsToRemove = Set[GoString]()
      for (otherColorString: GoString <- adjacentOppositeColor) {
        val otherString = otherColorString.withoutLiberty(point)
        if (otherString.numLiberties > 0) {
          newGrid = replaceString(otherString, newGrid)
        } else stringsToRemove += otherString
      }

      var newBlackCaptures = blackCaptures
      var newWhiteCaptures = whiteCaptures
      stringsToRemove.foreach(str => {
        player match {
          case BlackPlayer => newBlackCaptures += str.size
          case WhitePlayer => newWhiteCaptures += str.size
        }
        val (nGrid, nHash) = removeString(str, newGrid, newHash)
        newGrid = nGrid
        newHash = nHash
      })

      GoBoard(size, newGrid, newHash, newBlackCaptures, newWhiteCaptures)
    }
  }

  /**
    * When a string is removed due to capture, also update the liberties of the adjacent strings of opposite color.
    * @param goString the string to remove
    * @return newGrid and newHash value
    */
  private def removeString(
      goString: GoString,
      grid: Map[Point, GoString],
      hash: Long
  ): (Map[Point, GoString], Long) = {
    var newGrid = grid
    var newHash = hash

    // first remove the stones from the board
    goString.stones.foreach { point =>
      newGrid -= point // the point is now empty
      newHash ^= ZOBRIST((point, Some(goString.player))) //Remove filled point hash code.
      newHash ^= ZOBRIST((point, None)) //Add empty point hash code.
    }

    // for each opponent neighbor string add a liberty for each adjacent removed point
    goString.stones.foreach { point =>
      neighborMap(point).foreach { neighbor =>
        val oppNbrString = getGoString(neighbor, newGrid)
        if (oppNbrString.nonEmpty) {
          newGrid = replaceString(oppNbrString.get.withLiberty(point), newGrid)
        }
      }
    }
    (newGrid, newHash)
  }

  private def replaceString(newString: GoString, grid: Map[Point, GoString]): Map[Point, GoString] = {
    var newGrid = grid
    for (point <- newString.stones)
      newGrid += (point -> newString)
    newGrid
  }

  def isSelfCapture(player: Player, point: Point): Boolean = {
    var friendlyStrings: List[GoString] = List[GoString]()

    for (neighbor <- neighborMap(point)) {
      getGoString(neighbor) match {
        case None                                                => return false
        case Some(friendNbrStr) if friendNbrStr.player == player => friendlyStrings :+= friendNbrStr
        case Some(oppNbrStr) if oppNbrStr.numLiberties == 1      => return false
        case _                                                   => new IllegalStateException("nbr=" + neighbor)
      }
    }

    friendlyStrings.forall(_.numLiberties == 1)
  }

  /** @return true if player playing at point will capture stones */
  def willCapture(player: Player, point: Point): Boolean =
    neighborMap(point).exists {
      getGoString(_) match {
        case Some(neighborString) if neighborString.player != player && neighborString.numLiberties == 1 => true
        case _                                                                                           => false
      }
    }

  def isOnGrid(point: Point): Boolean = 1 <= point.row && point.row <= size && 1 <= point.col && point.col <= size

  def getPlayer(point: Point): Option[Player] = grid.get(point).map(_.player)

  def getGoString(point: Point, myGrid: Map[Point, GoString] = grid): Option[GoString] = myGrid.get(point)

  override def toString: String = serializer.serialize()
}
