package com.rigobertocanseco.sudoku

import com.rigobertocanseco.sudoku.core.{Grid, Sudoku}
import com.rigobertocanseco.sudoku.game.SudokuGame
import org.gillius.jalleg.binding.AllegroLibrary
import org.gillius.jalleg.binding.AllegroLibrary.*
import org.gillius.jalleg.framework.{AllegroAddon, Game}
import org.gillius.jalleg.binding.ALLEGRO_COLOR
import org.gillius.jalleg.binding.ALLEGRO_FONT

import com.rigobertocanseco.sudoku.core.print

@main
def main(): Unit =
  println("Hello world!")
  val sudoku = Sudoku()
  val array: Array[Array[Int]] = sudoku.readFile("src/main/resources/sudoku2.txt")
  val grid: Grid = sudoku.arrayToGrid(array)
  grid.print()
  new SudokuGame().run()

end main