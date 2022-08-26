package com.rigobertocanseco.sudoku

/**
 * Sudoku Puzzle in Scala 3.0
 */

import scala.io.Source
//import scala.collection.parallel.CollectionConverters._

enum Num {
  case ZERO, ONE, TWO, TREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE
}

case class Options(set: Set[Num])
case class Cell(value: Num, options: Options)
case class Grid(array: Array[Array[Cell]])
case class Position(row: Int, col: Int)
case class Pair(cell: Cell, position: Position)
trait SudokuSolver {
  def solve(grid: Grid): Grid
}
/**
 * The Strategy Naked Single:
 * If a cell only has a single candidate, that candidate solves the cell. This is obvious: if there are no other
 * possible candidates in a cell, the only one present must be it.
 */
class NakedSolver extends SudokuSolver {
  def solve(grid: Grid): Grid = ???
}

object Sudoku {
  val size = 9
  val numbers: Array[Num] = Array[Num](Num.values: _*)
}

class Sudoku(val size: Int) {
  def this() = this(Sudoku.size)
  def readFile(src: String): Array[Array[Int]] =
    Source.fromFile(src)
      .map(_.getNumericValue)
      .filter(e => e != -1)
      .toArray
      .grouped(size)
      .toArray

  def arrayToGrid(a: Array[Array[Int]]): Grid =
    Grid(a.map(r => r.map(c => Cell(Num.values.apply(c), Options(Set())))))

}

extension (a: Array[Num]) {
  def difference(b: Array[Num]): Array[Num] =
    a.filter(n => !b.contains(n)).filter(n => n != Num.ZERO)
}

extension (a: Array[Cell]) {
  def getValues: Array[Num] = a.map(cell => cell.value).filter(e => e != Num.ZERO)
  def getOptions: Array[Num] = a.map(cell => cell.options.set).reduce(_ union _).toArray
  def union(b: Array[Cell]): Array[Cell] =
    a.concat(b).distinct
}

extension (a: Array[Pair]) {
  def valueEqualTo(n: Num): Array[Pair] = a.filter(cell => cell.cell.value == n)
  def index(): Array[Position] = a.map(_.position)
}

extension (g: Grid) {
  def row(row: Int): Array[Cell] = g.array(row)
  def col(col: Int): Array[Cell] = g.array.map(row => row(col))
  def square(row: Int, col: Int): Array[Cell] = {
    val sqrt = Math.sqrt(Sudoku.size).toInt
    val rowStart = (row / sqrt) * sqrt
    val colStart = (col / sqrt) * sqrt
    val rowEnd = rowStart + sqrt
    val colEnd = colStart + sqrt

    g.array.slice(rowStart, rowEnd).flatMap(row => row.slice(colStart, colEnd))
  }
  def options(): Grid =
    Grid(g.zip().map(pair => pair.cell.value match {
      case Num.ZERO =>
        Cell(Num.ZERO, Options(Set(Sudoku.numbers.difference(g.row(pair.position.row)
          .union(g.col(pair.position.col))
          .union(g.square(pair.position.row, pair.position.col))
          .getValues): _*))
        )
      case _ => pair.cell
    }).grouped(Sudoku.size).toArray)

  def options(r: Int, c: Int): Options =
    Options(Set(Sudoku.numbers.difference(g.row(r).union(g.col(c)).union(g.square(r, c)).getValues): _*))

  def zip(): Array[Pair] =
    g.array.zipWithIndex.flatMap(row => row(0).zipWithIndex.map(cell => Pair(cell(0), Position(row(1), cell(1)))))

  def print(): Unit = g.array.foreach(row => println(row.map(cell => cell.value.toString).mkString(" ")))

  def nakedSimple(): Grid = {
    val grid = g.options()
    grid.zip()
      .filter(pair => pair.cell.options.set.size == 1)
      .foreach(e => grid.array(e.position.row)(e.position.col) = Cell(e.cell.options.set.head, Options(Set())))
    grid
  }
}