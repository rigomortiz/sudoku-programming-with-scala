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
  val s1: List[Cell] = grid.options().array.flatMap(_.filter(_.options.set.size == 1)).toList
  s1.foreach(pair => println(s"Cell ${pair} has only one option: ${pair.options.set.head}"))

  println("The Strategy Unique")
  val s2: List[Cell] = (0 until 9).map(i => {
    (grid.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1),
      grid.options().col(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1),
      grid.options().row(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1))
    }).flatMap(e => e._1.map(e => (e._1, e._2.head)) ++ e._2.map(e => (e._1, e._2.head)) ++ e._3.map(e => (e._1, e._2.head)))
    .toList.distinct.map(e => Cell(e._1, Options(Set()), e._2))
  s2.foreach(cell => println(s"Cell ${cell.position} has only one option in its box, row or column: ${cell.value}"))

  println("The Strategy Cleanup")
  val grid2 = grid.cleanup(s2)
  grid2.sprint()

  println("The Strategy Naked Pair")
  (0 until 9).map(i => {
    (grid2.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
      .map(e => NumPositions(e._1, e._2: _*))
      .groupBy(e => (e.positions(0), e.positions(1)))
      .filter(e => e._2.toList.length == 2)
      .map(e => ("box", e._1, Options(e._2.map(_.num).toSet))),
      grid2.options().row(i).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
        .map(e => NumPositions(e._1, e._2: _*))
        .groupBy(e => (e.positions(0), e.positions(1)))
        .filter(e => e._2.toList.length == 2)
        .map(e => ("row", e._1, Options(e._2.map(_.num).toSet))),
      grid2.options().col(i).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
        .map(e => NumPositions(e._1, e._2: _*))
        .groupBy(e => (e.positions(0), e.positions(1)))
        .filter(e => e._2.toList.length == 2)
        .map(e => ("col", e._1, Options(e._2.map(_.num).toSet)))
      )
  }).flatMap(e => e._1.map(e => (e._1, e._2, e._3)) ++ e._2.map(e => (e._1, e._2, e._3)) ++ e._3.map(e => (e._1, e._2, e._3)))
    .toList
    .foreach(e => println(s"${e._1} Positions ${e._2} has only two options: ${e._3}"))


end main