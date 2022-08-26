/**
 * Sudoku Puzzle in Scala 3.0
 */

import scala.io.Source
import scala.collection.parallel.CollectionConverters._
println("SuDoku Puzzle Solver with Scala 3.0")
enum Num {
  case ZERO, ONE, TWO, TREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE
}
case class Options(set: Set[Num])
case class Cell(value: Num, options: Options)
case class Grid(array: Array[Array[Cell]])
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

  def solve(sudoku: Sudoku): Grid = ???
  
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

extension (a: Array[(Cell, (Int, Int))]) {
  def valueEqualTo(n: Num): Array[(Cell, (Int, Int))] = a.filter(cell => cell(0).value == n)
  def index(): Array[(Int, Int)] = a.map(_(1))
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
    Grid(g.zip().map(e => e(0).value match {
      case Num.ZERO =>
        Cell(Num.ZERO, Options(Set(Sudoku.numbers.difference(g.row(e(1)(0))
          .union(g.col(e(1)(1)))
          .union(g.square(e(1)(0), e(1)(1)))
          .getValues): _*))
        )
      case _ => e(0)
    }).grouped(Sudoku.size).toArray)

  def options(r: Int, c: Int): Options =
    Options(Set(Sudoku.numbers.difference(g.row(r).union(g.col(c)).union(g.square(r, c)).getValues): _*))

  def zip(): Array[(Cell, (Int, Int))] =
    g.array.zipWithIndex.flatMap(row => row(0).zipWithIndex.map(cell => (cell(0), (row(1), cell(1)))))

  def print(): Unit = g.array.foreach(row => println(row.map(cell => cell.value.toString).mkString(" ")))

}

val sudoku = new Sudoku()
val array = sudoku.readFile("C:\\Users\\w10\\Documents\\Projects\\sudoku\\src\\main\\resources\\sudoku.txt")
val grid: Grid = sudoku.arrayToGrid(array)

val zeros = grid.zip().valueEqualTo(Num.ZERO)
zeros.index().mkString("\n")

val options = grid.options()
options.print()

for (i <- 0 until Sudoku.size) {
  for (j <- 0 until Sudoku.size) {
    println(s"$i, $j : ${options.array(i)(j)}")
  }
}

println("Options equals to one:")
grid.zip().filter(e => e(1)(0) == 0).map(_(0).options.set).filter(_.size == 1)

grid.zip().filter(e => e(1)(0) == 0)

grid.zip()

grid.zip().filter(e => e(0).options.set.size == 1)