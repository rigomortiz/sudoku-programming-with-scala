package com.rigobertocanseco.sudoku

import com.rigobertocanseco.sudoku.core._
import com.rigobertocanseco.sudoku.game.SudokuGame
import org.gillius.jalleg.binding.AllegroLibrary
import org.gillius.jalleg.binding.AllegroLibrary.*
import org.gillius.jalleg.framework.{AllegroAddon, Game}
import org.gillius.jalleg.binding.ALLEGRO_COLOR
import org.gillius.jalleg.binding.ALLEGRO_FONT

@main
def main(): Unit =
  val sudoku = Sudoku()
  val array: Array[Array[Int]] = sudoku.readFile("src/main/resources/sudoku2.txt")
  val grid: Grid = sudoku.arrayToGrid(array)
  println("Sudoku")
  grid.sprint()

  println("The Strategy Naked Single")
  val s1: List[Cell] = grid.nakedSingle
  s1.foreach(pair => println(s"Cell ${pair} has only one option: ${pair.options.set.head}"))

  println("The Strategy Unique")
  val s2: List[Cell] = grid.uniques
  s2.foreach(cell => println(s"Cell ${cell.position} has only one option in its box, row or column: ${cell.value}"))

  println("The Strategy Cleanup")
  val grid2 = grid.cleanup(s2)
  grid2.sprint()

  println("The Strategy Naked Pair")
  grid2.nakedPair.foreach(e => println(s"${e._1} Positions ${e._2} has only two options: ${e._3}"))

  val grid4 = grid2.updateBox(8, Array(
    Cell(0, Options(Set()), Position(0, 0)),
    Cell(0, Options(Set()), Position(0, 1)),
    Cell(0, Options(Set()), Position(0, 2)),
    Cell(0, Options(Set()), Position(0, 3)),
    Cell(0, Options(Set()), Position(0, 4)),
    Cell(0, Options(Set()), Position(0, 5)),
    Cell(0, Options(Set()), Position(0, 6)),
    Cell(0, Options(Set()), Position(0, 7)),
    Cell(0, Options(Set()), Position(0, 8))))
  grid4.sprint()

end main