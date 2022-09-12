package com.rigobertocanseco.sudoku.core

/**
 * Sudoku Puzzle in Scala 3.0
 */

import scala.io.Source
//import scala.collection.parallel.CollectionConverters._

/*enum Num {
  case ZERO, ONE, TWO, TREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE
}*/
type Num = Int
final val ZERO = 0

case class Options(set: Set[Num])
case class Cell(value: Num, options: Options)
case class Grid(array: Array[Array[Cell]])
case class Position(row: Int, col: Int)
case class Pair(cell: Cell, position: Position)
trait Strategy {
  def apply(grid: Grid): Grid
}
/**
 * The Strategy Naked Single:
 * If a cell only has a single candidate, that candidate solves the cell. This is obvious: if there are no other
 * possible candidates in a cell, the only one present must be it.
 */
class NakedSolver extends Strategy {
  def apply(grid: Grid): Grid = {
    val g = grid
      grid.options().zip()
      .filter(pair => pair.cell.options.set.size == 1)
      .foreach(e => g.array(e.position.row)(e.position.col) = Cell(e.cell.options.set.head, Options(Set())))
    g
  }
}

object Sudoku {
  val size = 9
  val numbers: Array[Num] = Array[Num](1 to 9 :_*)
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
    Grid(a.map(r => r.map(c => Cell(c, Options(Set())))))

}

extension (a: Array[Num]) {
  def difference(b: Array[Num]): Array[Num] =
    a.filter(n => !b.contains(n)).filter(n => n != ZERO)
}

extension (a: Array[Cell]) {
  def getValues: Array[Num] = a.map(cell => cell.value).filter(e => e != ZERO)
  def getOptions: Array[Num] = a.map(cell => cell.options.set).reduce(_ union _).toArray
  def union(b: Array[Cell]): Array[Cell] =
    (a ++ b).distinct
}

extension (a: Array[Pair]) {
  def valueEqualTo(n: Num): Array[Pair] = a.filter(cell => cell.cell.value == n)
  def index(): Array[Position] = a.map(_.position)
  def col(col: Int): Array[Pair] = a.filter(_.position.col == col)
  def row(row: Int): Array[Pair] = a.filter(_.position.row == row)
  def box(num: Int): Array[Pair] = {
    val row = (num / 3) * 3
    val col = (num % 3) * 3
    a.filter(p => p.position.row >= row && p.position.row < row + 3 && p.position.col >= col && p.position.col < col + 3)
  }
  def uniqueOption(): Array[Pair] = a.filter(pair => pair.cell.options.set.size == 1)
  /*def pair(): Array[Pair] =
    val pair = a.filter(pair => pair.cell.options.set.size == 2)
    if pair.length == 2 then pair else Array()*/

  def emptyCells(): Array[Pair] = a.filter(_.cell.value == ZERO)
  def groupBy(): List[Map[Num, Array[Pair]]] =
    a.flatMap(e => e.cell.options.set.map(i => (i, e)))
    .groupBy(e => e(0))
    .map(e => Map(e._1 -> e._2.map(e => e._2)))
    .toList
  def groupByOption(): List[Map[Num, Array[Position]]] =
    a.flatMap(e => e.cell.options.set.map(i => (i, e.position)))
    .groupBy(e => e(0))
    .map(e => Map(e._1 -> e._2.map(e => e._2)))
    .toList
  def groupByPosition(): List[Map[Position, Array[Num]]] =
    a.flatMap(e => e.cell.options.set.map(i => (i, e.position)))
    .groupBy(e => e(1))
    .map(e => Map(e._1 -> e._2.map(e => e._1)))
    .toList
}

extension (l: List[Map[Num, Array[Position]]]) {
  def unique: Array[(Num, Array[Position])] = l.flatMap(e => e.map(e => (e._1,e._2)).filter(_._2.length == 1)).toArray
  //def pair(): Array[(Num, Array[Position])] = l.flatMap(e => e.map(e => (e._1,e._2)).filter(_._2.length == 2)).toArray
}

extension (l: List[Map[Position, Array[Num]]]) {
  //def pair(): Array[(Position, Array[Num])] = l.flatMap(e => e.map(e => (e._1,e._2)).filter(_._2.length == 2)).toArray
  /*def pair(): Array[(Position, Array[Num])] =
    val p = l.flatMap(e => e.map(e => (e._1, e._2)).filter(_._2.length == 2)).toArray
    if p.length == 2 && p(0)(0) == p(1)(0) && p(0)(1) == p(1)(1)  then p else Array()*/
  def p = ???
}

extension (l:  List[Map[Num, Array[Pair]]]) {
  def pair(): Array[Pair] = l.flatMap(e => e.map(e => (e._1,e._2)).filter(_._2.length == 2)).toArray
    .flatMap(e => e._2.filter( i => i.cell.options.set.size == 2))
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
      case ZERO =>
        Cell(ZERO, Options(Set(Sudoku.numbers.difference(
          g.row(pair.position.row)
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

  def sprint(): Unit = {
    for (row <- g.array) {
      for (cell <- row) {
        if cell.value == ZERO then
          print(cell.options.set.mkString("(", " ", ")"))
        else
          print(s"${cell.value}")
        printf("\t")
      }
      println()
    }
    /*g.array.foreach( row => {
      if row(0).value == ZERO then
        row.map(cell => cell.options.set).grouped(3).map(_.mkString(",")) foreach println
      else println(row.map(cell => cell.value).mkString(" "))
    })*/
  }

  def strategyNakedSimple(): Grid = {
    val grid = g.options()
    grid.zip()
      .filter(pair => pair.cell.options.set.size == 1)
      .foreach(e => grid.array(e.position.row)(e.position.col) = Cell(e.cell.options.set.head, Options(Set())))
    grid
  }

  def strategyUnique(): Grid = {
    val grid = g.options()
      grid.zip()
        .filter(p => p.position.col == 1)
        .filter(p => p.cell.value == ZERO)
        .flatMap(e => e.cell.options.set.map(i => (i, e.position)))
        .groupBy(e => e(0))
        .filter(e => e(1).size == 1)
        .map(e => e(1))
    grid
  }
}
