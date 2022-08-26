@main
def main(): Unit =
  println("Hello world!")
  val sudoku = Sudoku()
  val array: Array[Array[Int]] = sudoku.readFile("src/main/resources/sudoku.txt")
  val grid: Grid = sudoku.arrayToGrid(array)
  //sudoku.printGrid(grid)
  val r0 = grid.row(0)
  val c1 = grid.col(1)
  val g1 = grid.square(0, 0)
  println(grid.row(0).mkString(" "))
  println(grid.col(1).getValues.mkString(" "))
  println(grid.square(0, 0).getValues.mkString(" "))
  val c00 = grid.row(0).union(grid.col(1)).union(grid.square(0, 0))
  println(grid.row(0).union(grid.col(1)).union(grid.square(0, 0)).getValues.mkString(" "))
  println(grid.row(0).union(grid.col(1)).union(grid.square(0, 0)).getOptions.mkString(" "))
  val c01 = Sudoku.numbers.difference(grid.row(0).union(grid.col(1)).union(grid.square(0, 0)).getValues)
  val zeros = grid.zip().valueEqualTo(Num.ZERO)
  //println(zeros.index().mkString("\n"))

  for (i <- grid.zip().valueEqualTo(Num.ZERO)) {
    println(s"${i}: ${grid.options(i(1)(0), i(1)(1)).set.mkString(" ")}")
  }

  Sudoku.numbers.foreach(n => println(s"$n: ${grid.zip().valueEqualTo(n).length}"))

  val g00 = grid.options(0, 1)
  println(g00.set.mkString(" "))

  for (i <- 0 until Sudoku.size) {
    for (j <- 0 until Sudoku.size) {
      println(s"$i, $j : ${grid.array(i)(j)}")
    }
  }

  val g = grid.options()
  for(i <- 0 until Sudoku.size) {
    for(j <- 0 until Sudoku.size) {
      println(s"$i, $j : ${g.array(i)(j)}")
    }
  }

  println("Options equals to one:")
  for (i <- grid.zip().filter(e => e(1)(0) == 0).map(_(0).options.set).filter(_.size == 1)) {
    println(s"${i}")
  }
  println("Options equals more that one:")
  for (i <- grid.zip()
    .map(e => e(0).options.set.map(s => (s, e(1))))) {
    println(s"${i}")
  }

end main