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
enum UnitSudoku {
  case ROW, COL, BOX
}
case class Options(set: Set[Num])
case class Position(row: Int, col: Int)
case class Cell(value: Num, options: Options, position: Position)
//case class Pair(cell: Cell, position: Position)
case class Grid(array: Array[Array[Cell]])
case class NumPositions(num: Num, positions: Position*)
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
    val array = (0 until 9).map(row => (0 until 9).map(col =>
      Cell(a(row)(col), Options(Set()), Position(row, col))
    ).toArray).toArray
    Grid(array)
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
  def unique(): Array[Cell] =
    a.filter(cell => cell.options.set.size == 1)

  def groupBy(): Map[Num, Array[Cell]] =
    a.flatMap(e => e.options.set.map(i => (i, e)))
      .groupBy(e => e(0))
      .flatMap(e => Map(e._1 -> e._2.map(e => e._2)))

  def groupByOption: Map[Num, Array[Position]] =
    a.flatMap(e => e.options.set.map(i => (i, e.position)))
      .groupBy(e => e(0))
      .flatMap(e => Map(e._1 -> e._2.map(e => e._2)))

  def groupByPosition(): Map[Position, Array[Num]] =
    a.flatMap(e => e.options.set.map(i => (i, e.position)))
      .groupBy(e => e(1))
      .flatMap(e => Map(e._1 -> e._2.map(e => e._1)))
  /*def pair(): Array[Pair] =
    val pair = a.filter(pair => pair.cell.options.set.size == 2)
    if pair.length == 2 then pair else Array()*/
  def emptyCells: Array[Cell] = a.filter(cell => cell.value == ZERO)
}

extension (m: Map[Num, Array[Position]]) {
  def uniqueOption: Map[Num, Array[Position]] = m.filter(pair => pair._2.length == 1)
  def pairOptions(u:UnitSudoku): List[(UnitSudoku, (Position, Position), Options)] =
    m.filter(pair => pair._2.length == 2)
    .map(e => NumPositions(e._1, e._2: _*))
    .groupBy(e => (e.positions(0), e.positions(1)))
    .filter(e => e._2.toList.length == 2)
    .map(e => (u, e._1, Options(e._2.map(_.num).toSet)))
    .toList
}

extension (a: Array[Array[Cell]]) {
  def row(row: Int): Array[Cell] = a(row)
  def col(col: Int): Array[Cell] = a.map(row => row(col))
  def box(num: Int): Array[Cell] = {
    val sqrt = Math.sqrt(Sudoku.size).toInt
    val row = (num / 3) * 3
    val col = (num % 3) * 3
    a.map(row => row.slice(col, col + sqrt)).slice(row, row + sqrt).flatten
  }
  def square(row: Int, col: Int): Array[Cell] = {
    val sqrt = Math.sqrt(Sudoku.size).toInt
    val rowStart = (row / sqrt) * sqrt
    val colStart = (col / sqrt) * sqrt
    val rowEnd = rowStart + sqrt
    val colEnd = colStart + sqrt

    a.slice(rowStart, rowEnd).flatMap(row => row.slice(colStart, colEnd))
  }
}


extension (g: Grid) {
  def options(): Array[Array[Cell]] =
    (0 until 9).map(row => (0 until 9).map(col =>
      if g.array(row)(col).value == ZERO then
        Cell(ZERO, options(row, col), Position(row, col))
      else g.array(row)(col)
    ).toArray).toArray

  def options(r: Int, c: Int): Options =
    Options(Set(Sudoku.numbers.difference(g.array.row(r).union(g.array.col(c)).union(g.array.square(r, c)).getValues): _*))

  def cleanup(l: List[Cell]): Grid = l match
    case Nil => g
    case head :: tail =>
      val row = head.position.row
      val col = head.position.col
      val value = head.value
      val options = head.options.set
      val cell = Cell(value, Options(options), Position(row, col))
      val array = g.array.updated(row, g.array(row).updated(col, cell))
      Grid(array).cleanup(tail)

  def removeOptions(l: List[(String, (Position, Position), Options)]): Grid = ???

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

  def nakedSingle: List[Cell] = g.options().array.flatMap(_.filter(_.options.set.size == 1)).toList

  def uniques: List[Cell] = (0 until 9).map(i => {
    (g.options().box(i).emptyCells.groupByOption.uniqueOption,
      g.options().col(i).emptyCells.groupByOption.uniqueOption,
      g.options().row(i).emptyCells.groupByOption.uniqueOption
    )})
    .flatMap(e => e._1.map(e => (e._1, e._2.head)) ++ e._2.map(e => (e._1, e._2.head)) ++ e._3.map(e => (e._1, e._2.head)))
    .toList.distinct.map(e => Cell(e._1, Options(Set()), e._2))

  def nakedPair: List[(UnitSudoku, (Position, Position), Options)] = (0 until 9).map(i => {
    (g.options().box(i).emptyCells.groupByOption.pairOptions(UnitSudoku.BOX),
      g.options().row(i).emptyCells.groupByOption.pairOptions(UnitSudoku.ROW),
      g.options().col(i).emptyCells.groupByOption.pairOptions(UnitSudoku.COL))
  }).flatMap(e => e._1.map(e => (e._1, e._2, e._3)) ++ e._2.map(e => (e._1, e._2, e._3)) ++ e._3.map(e => (e._1, e._2, e._3)))
    .toList

  def updateRow(row: Int, cells: Array[Cell]): Grid = {
    val array = g.array.clone()
    array(row) = cells
    Grid(array)
  }

  def updateCol(col: Int, cells: Array[Cell]): Grid = {
    val array = g.array.clone()
    array.map(row => {
      row(col) = cells(col)
      row
    })
    Grid(array)
  }

  def updateBox(num: Int, cells: Array[Cell]): Grid = {
    val sqrt = Math.sqrt(Sudoku.size).toInt
    val row = (num / 3) * 3
    val col = (num % 3) * 3
    val array = g.array.clone()
    cells.grouped(sqrt).zipWithIndex.foreach(e => {
      array(row + e._2).update(col, e._1(0))
      array(row + e._2).update(col + 1, e._1(1))
      array(row + e._2).update(col + 2, e._1(2))
    })
    Grid(array)
  }
}