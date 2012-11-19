import collection.parallel.immutable.ParVector
import collection.parallel.mutable.ParArray

/**
 * Created by Mitchell Vanderhoeff
 * Date: 2012-11-18
 * Time: 5:49 PM
 */

object LifeRules {
  val live = Some(true)
  val die = Some(false)
  val doNothing = None
}

case class Cell(x:Int, y:Int, alive:Boolean = false) {
  def dead = !alive

  def isNeighbourWith(other:Cell):Boolean = {
    math.abs(other.x - this.x) + math.abs(other.y - this.x) < 2
  }
}

class World(width:Int, height:Int, lifeRules:((Cell, ParVector[Cell]) => Option[Boolean])) {
  val EMPTY_CELLS:ParVector[Cell] = {
    ParVector.tabulate(width * height) {
      (index: Int) =>
        val x = index % width
        val y = math.floor(index / width).toInt
        Cell(x, y)
    }
  }

  var cells:ParVector[Cell] = EMPTY_CELLS

  def clear() {
    cells = EMPTY_CELLS
  }

  def randomize(ratio:Double = 0.5) {
    cells = cells.map( (cell: Cell) =>
      Cell(cell.x, cell.y, alive = (math.random < ratio))
    )
  }

  def findNeighbours(targetCell: Cell): ParVector[Cell] = {
    cells.filter((cell: Cell) => targetCell.isNeighbourWith(cell))
  }

  def tick() {
    cells = cells.map {
      (cell: Cell) =>
        val rulesResult = lifeRules.apply(cell, findNeighbours(cell))
        if (rulesResult.isDefined)
          Cell(cell.x, cell.y, rulesResult.get)
        else
          Cell(cell.x, cell.y, cell.alive)
    }
  }

  def run(times:Int = 10) {
    display()
    for (i <- 0 until times) {
      tick()
      display()
    }
  }

  def display() {
    val cellStringMatrix = ParArray.fill(height, width)("")
    cells.foreach {
      (cell: Cell) =>
        cellStringMatrix(cell.y)(cell.x) = if (cell.alive) "o" else " "
    }

    val horizontalLine:String = " " + ("-" * width) + " "

    val cellStringRows =
        cellStringMatrix.map (
          (row: ParArray[String]) => f"|${row.mkString}|"
        )

    println(horizontalLine)
    cellStringRows.foreach(println(_))
    println(horizontalLine)
  }
}
