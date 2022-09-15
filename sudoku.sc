
/**
 * Sudoku Puzzle in Scala 3.0
 */

import scala.io.Source
//import scala.collection.parallel.CollectionConverters._

/*enum Num {
  case ZERO, ONE, TWO, TREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE
}*/
type Num = Int
val ZERO = 0

case class Options(set: Set[Num])
case class Position(row: Int, col: Int)
case class NumPositions(num: Num, positions: Position*)

case class Cell(value: Num, options: Options, position: Position)
//case class Pair(cell: Cell, position: Position)
case class Grid(array: Array[Array[Cell]])
trait Strategy {
  def apply(grid: Grid): Grid
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

  def groupByOption(): Map[Num, Array[Position]] =
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
  def emptyCells(): Array[Cell] = a.filter(cell => cell.value == ZERO)

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

  // def zip(): Array[Cell] =
  //  g.array.zipWithIndex.flatMap(row => row(0).zipWithIndex.map(cell => Pair(cell(0), Position(row(1), cell(1)))))


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
}



val sudoku = new Sudoku()
val array = sudoku.readFile("C:\\Users\\w10\\Documents\\Projects\\sudoku\\src\\main\\resources\\sudoku2.txt")
val grid: Grid = sudoku.arrayToGrid(array)
(0 until 9).map(i => {
  val rows = grid.options().row(i).emptyCells().groupByOption().filter(pair => pair._2.size == 1)
  val cols = grid.options().col(i).emptyCells().groupByOption().filter(pair => pair._2.size == 1)
  val boxs = grid.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.size == 1)
  (rows, cols, boxs)
})

for (i <- 0 until 9)
  val v: Map[Num, Array[Position]] = grid.options().row(i).emptyCells()
    .groupByOption()
  println(s"Row ${i}:")
  v.foreach(e => {
    println(s"\tNum ${e._1} has ${e._2.length} options:")
    e._2.foreach(e => println(s"\t\t${e}"))
  })

for (i <- 0 until 9)
  val v: Map[Num, Array[Position]] = grid.options().col(i).emptyCells()
    .groupByOption()
  println(s"Col ${i}:")
  v.foreach(e => {
    println(s"\tNum ${e._1} has ${e._2.length} options:")
    e._2.foreach(e => println(s"\t\t${e}"))
  })

for (i <- 0 until 9)
  val v: Map[Num, Array[Position]] = grid.options().box(i).emptyCells()
    .groupByOption()
  println(s"Box ${i}:")
  v.foreach(e => {
    println(s"\tNum ${e._1} has ${e._2.length} options:")
    e._2.foreach(e => println(s"\t\t${e}"))
  })

println("Box")
(0 until 9).toList.map(i => grid.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.size == 1))
  .foreach(println)


(0 until 9).toList.map(i => grid.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.size == 1))

(0 until 9).toList.map(i => grid.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.size == 1))
  .flatMap(e => e.map(e => (e._1, e._2.head)))

grid.options().flatMap(_.filter(_.options.set.size == 1))

grid.options()

grid.array.square(0, 0)
grid.options(0,0)
grid.options(0,1)
grid.array.square(0, 1)


(0 until 9).map(
  i => {
    val b = grid.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1)
    val c = grid.options().col(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1)
    val r = grid.options().row(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1)
    (b, c, r)
  }
)
  .flatMap(e => e._1.map(e => (e._1, e._2.head))
    ++ e._2.map(e => (e._1, e._2.head))
    ++ e._3.map(e => (e._1, e._2.head)))
  .toList.distinct

grid.options().array.flatMap(_.filter(_.options.set.size == 1))

grid.options().box(3).emptyCells().groupByOption().filter(pair => pair._2.length == 2)

grid.options().box(3).emptyCells().groupByOption()

grid.options().box(3).emptyCells().groupByOption().filter(pair => pair._2.length == 1)

(0 until 9).map(i => {
  (grid.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1),
    grid.options().col(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1),
    grid.options().row(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1))
}).flatMap(e => e._1.map(e => (e._1, e._2)) ++ e._2.map(e => (e._1, e._2)) ++ e._3.map(e => (e._1, e._2)))
  .toList.distinct.map(e => Cell(e._1, Options(Set()), e._2))
  .foreach(cell => println(s"Cell ${cell.position} has only one option in its box, row or column: ${cell.value}"))

(0 until 9).map(i => {
  (grid.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1),
    grid.options().col(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1),
    grid.options().row(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1))
}).flatMap(e => e._1.map(e => (e._1, e._2.head)) ++ e._2.map(e => (e._1, e._2.head)) ++ e._3.map(e => (e._1, e._2.head)))
  .toList//.distinct.map(e => Cell(e._1, Options(Set()), e._2))
//.foreach(cell => println(s"Cell ${cell.position} has only one option in its box, row or column: ${cell.value}"))


(0 until 9).map(i => {
  (grid.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1),
    grid.options().col(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1),
    grid.options().row(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1))
}).flatMap(e => e._1.map(e => (e._1, e._2)) ++ e._2.map(e => (e._1, e._2)) ++ e._3.map(e => (e._1, e._2)))
  .toList

(0 until 9).map(i => {
  (grid.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.length == 2),
    grid.options().col(i).emptyCells().groupByOption().filter(pair => pair._2.length == 2),
    grid.options().row(i).emptyCells().groupByOption().filter(pair => pair._2.length == 2))
}).flatMap(e => e._1.map(e => (e._1, e._2)) ++ e._2.map(e => (e._1, e._2)) ++ e._3.map(e => (e._1, e._2)))
  .toList


(0 until 9).map(i => {
  (grid.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1),
    grid.options().col(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1),
    grid.options().row(i).emptyCells().groupByOption().filter(pair => pair._2.length == 1))
}).flatMap(e => e._1.map(e => (e._1, e._2)) ++ e._2.map(e => (e._1, e._2)) ++ e._3.map(e => (e._1, e._2)))
  .toList.distinct

(0 until 9).map(i => {
  (grid.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.length == 2),
    grid.options().col(i).emptyCells().groupByOption().filter(pair => pair._2.length == 2),
    grid.options().row(i).emptyCells().groupByOption().filter(pair => pair._2.length == 2))
}).flatMap(e => e._1.map(e => (e._1, e._2)) ++ e._2.map(e => (e._1, e._2)) ++ e._3.map(e => (e._1, e._2)))
  .toList.distinct

grid.options().box(3).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().box(0).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().box(1).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().box(2).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().box(4).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().box(5).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().box(7).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().box(6).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().box(8).emptyCells().groupByOption().filter(pair => pair._2.length == 2)

grid.options().col(0).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().col(1).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().col(2).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().col(3).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().col(4).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().col(5).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().col(6).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().col(7).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().col(8).emptyCells().groupByOption().filter(pair => pair._2.length == 2)


grid.options().row(0).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().row(1).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().row(2).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().row(3).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().row(4).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().row(5).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().row(6).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().row(7).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().row(8).emptyCells().groupByOption().filter(pair => pair._2.length == 2)

val s1 = grid.options().array.flatMap(_.filter(_.options.set.size == 1)).toList

grid.options().box(7).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
grid.options().box(7).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
  .toList

grid.options().box(7).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
  .map(e => NumPositions(e._1, e._2: _*))
  .groupBy(e => (e.positions(0), e.positions(1)))
  .filter(e => e._2.toList.length == 2)
  .map(e => (e._1, Options(e._2.map(_.num).toSet)))

grid.options().box(7).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
  .map(e => NumPositions(e._1, e._2: _*))
  .groupBy(e => (e.positions(0), e.positions(1)))
  .filter(e => e._2.toList.length == 2)
  .map(e => (e._1, Options(e._2.map(_.num).toSet)))
  .toList

println("Box 7")
grid.options().box(7).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
  .map(e => NumPositions(e._1, e._2: _*))
  .groupBy(e => (e.positions(0), e.positions(1)))
  .filter(e => e._2.toList.length == 2)
  .map(e => (e._1, Options(e._2.map(_.num).toSet)))
  .toList
  .foreach(cell => println(s"Cell ${cell._1}, ${cell._2}"))


println("The Strategy Naked Pair")
(0 until 9).map(i => {
  (grid.options().box(i).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
    .map(e => NumPositions(e._1, e._2: _*))
    .groupBy(e => (e.positions(0), e.positions(1)))
    .filter(e => e._2.toList.length == 2)
    .map(e => ("box", e._1, Options(e._2.map(_.num).toSet))),
    grid.options().row(i).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
      .map(e => NumPositions(e._1, e._2: _*))
      .groupBy(e => (e.positions(0), e.positions(1)))
      .filter(e => e._2.toList.length == 2)
      .map(e => ("row", e._1, Options(e._2.map(_.num).toSet))),
    grid.options().col(i).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
      .map(e => NumPositions(e._1, e._2: _*))
      .groupBy(e => (e.positions(0), e.positions(1)))
      .filter(e => e._2.toList.length == 2)
      .map(e => ("col", e._1, Options(e._2.map(_.num).toSet)))
  )
}).flatMap(e => e._1.map(e => (e._1, e._2, e._3)) ++ e._2.map(e => (e._1, e._2, e._3)) ++ e._3.map(e => (e._1, e._2, e._3)))
  .toList

grid.options().row(6).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
  .map(e => NumPositions(e._1, e._2: _*))
  .groupBy(e => (e.positions(0), e.positions(1)))
  .filter(e => e._2.toList.length == 2)
  .map(e => ("box", e._1, Options(e._2.map(_.num).toSet)))

grid.options().row(6).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
  .map(e => NumPositions(e._1, e._2: _*))
  .groupBy(e => (e.positions(0), e.positions(1)))
  .filter(e => e._2.toList.length == 2)
  .map(e => ("box", e._1, Options(e._2.map(_.num).toSet)))
  .toList

grid.options().row(6).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
  .map(e => NumPositions(e._1, e._2: _*))
  .groupBy(e => (e.positions(0), e.positions(1)))
  .filter(e => e._2.toList.length == 2)


grid.options().row(6).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
  .map(e => NumPositions(e._1, e._2: _*))
  .groupBy(e => (e.positions(0), e.positions(1)))
  .filter(e => e._2.toList.length == 2)
  .map(e => ("box", e._1, Options(e._2.map(_.num).toSet)))
  .toList
  
val c = grid.options().row(6).emptyCells().groupByOption().filter(pair => pair._2.length == 2)
  .map(e => NumPositions(e._1, e._2: _*))
  .groupBy(e => (e.positions(0), e.positions(1)))
  .filter(e => e._2.toList.length == 2)
  .map(e => ("box", e._1, Options(e._2.map(_.num).toSet)))
  .toList

