package com.rigobertocanseco.sudoku

import com.rigobertocanseco.sudoku.core.{Grid, NakedSolver, Sudoku, sprint, options}
import com.rigobertocanseco.sudoku.game.SudokuGame
import org.gillius.jalleg.binding.AllegroLibrary
import org.gillius.jalleg.binding.AllegroLibrary.*
import org.gillius.jalleg.framework.{AllegroAddon, Game}
import org.gillius.jalleg.binding.ALLEGRO_COLOR
import org.gillius.jalleg.binding.ALLEGRO_FONT

@main
def main(): Unit =
  println("Hello world!")
  val sudoku = Sudoku()
  val array: Array[Array[Int]] = sudoku.readFile("src/main/resources/sudoku2.txt")
  val grid: Grid = sudoku.arrayToGrid(array)
  grid.options().sprint()
  val nakedSolver = new NakedSolver()
  val g = nakedSolver.apply(grid)
  //println("Sudoku")
  //new SudokuGame().run()

end main