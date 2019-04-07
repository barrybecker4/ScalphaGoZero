package org.deeplearning4j.scalphagozero.agents

import java.util.Scanner

import org.deeplearning4j.scalphagozero.board._
import HumanAgent.INT_REGEX

/**
  * A human agent allows a real person to select the next move in a go game.
  *
  * @author Barry Becker
  */
class HumanAgent() extends Agent {

  val scanner = new Scanner(System.in)

  override def selectMove(gameState: GameState): Move = {
    println("Enter the coordinates (row, col) of where you would like to play (or P to pass, and R to resign): ")
    var text: String = ""
    var valid = false
    do {
      text = scanner.nextLine()
      valid = validMove(text)
      if (!valid) {
        println(text + " is not a valid coordinate. Try again.")
      }
    } while (!valid)

    text match {
      case "P" => Move.Pass
      case "R" => Move.Resign
      case s: String =>
        val a = s.split(',')
        Move.Play(Point(a(0).trim.toInt, a(1).trim.toInt))
    }
  }

  private def validMove(text: String): Boolean = {
    if (text == "R" || text == "P")
      return true
    if (text.contains(",")) {
      val a = text.split(',')
      println("first = " + a(0) + " second = " + a(1))
      return validInt(a(0)) && validInt(a(1))
    }
    false
  }

  private def validInt(txt: String): Boolean = {
    val result = txt.trim match {
      case INT_REGEX(str) => str.toInt
      case _              => -1
    }
    result > 0
  }
}

object HumanAgent {
  private val INT_REGEX = """(\d+)""".r

  def main(args: Array[String]): Unit = {
    val agent = new HumanAgent()
    val gameState = new GameState(GoBoard(9), BlackPlayer)
    val move = agent.selectMove(gameState)
    println(move)
  }
}
