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
  def spawn(isAlive:Boolean) = Cell(x, y, isAlive)

  def dead = !alive

  def isE(other:Cell):Boolean =   (other.x - this.x == 1)   && (other.y == this.y)
  def isNE(other:Cell):Boolean =  (other.x - this.x == 1)   && (other.y - this.y == 1)
  def isN(other:Cell):Boolean =   (other.x == this.x)       && (other.y - this.y == 1)
  def isNW(other:Cell):Boolean =  (other.x - this.x == -1)  && (other.y - this.y == 1)
  def isW(other:Cell):Boolean =   (other.x - this.x == -1)  && (other.y == this.y)
  def isSW(other:Cell):Boolean =  (other.x - this.x == -1)  && (other.y - this.y == -1)
  def isS(other:Cell):Boolean =   (other.x == this.x)       && (other.y - this.y == -1)
  def isSE(other:Cell):Boolean =  (other.x - this.x == 1)   && (other.y - this.y == -1)

  def isNeighbourWith(other:Cell):Boolean = {
    isE(other) || isNE(other) || isN(other) || isNW(other) ||
    isW(other) || isSW(other) || isS(other) || isSE(other)
  }
}

class World(width:Int, height:Int, lifeRules:((Cell, List[Cell]) => Option[Boolean])) {
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

  def spawnCellsAt(points:(Int, Int)*) {
    cells = cells.map { cell: Cell =>
      if (points.exists( (point: (Int, Int)) => point == (cell.x, cell.y) )) {
        cell.spawn(isAlive = true)
      } else {
        cell
      }
    }
  }

  def randomize(ratio:Double = 0.5) {
    cells = cells.map( (cell: Cell) =>
      cell.spawn(isAlive = (math.random < ratio))
    )
  }

  def findNeighbours(targetCell: Cell): List[Cell] = {
    cells.filter((cell:Cell) => cell.alive && targetCell.isNeighbourWith(cell)).toList
  }

  def tick() {
    cells = cells.map {
      (cell: Cell) =>
        val rulesResult = lifeRules.apply(cell, findNeighbours(cell))
        if (rulesResult.isDefined)
          cell.spawn(rulesResult.get)
        else
          cell
    }
  }

  def run(times:Int = 10) {
    val displayInterval = math.floor(times / 10).toInt
    display()
    for (i <- 0 until times) {
      tick()
      if (i % displayInterval == 0)
        display()
    }
  }

  def display() {
    val displayMatrix:ParArray[ParArray[Cell]] = ParArray.fill(height, width)(null)

    cells.foreach { (cell: Cell) =>
        displayMatrix(cell.y)(cell.x) = cell
    }

    val displayRowsOrdered = displayMatrix.seq.sortBy(_.head.y).reverse

    val horizontalLine:String = " " + ("-" * width) + " "

    println(horizontalLine)

    displayRowsOrdered.foreach {
      (row: ParArray[Cell]) =>
        val rowStr = row.map( (cell: Cell) => if (cell.alive) "o" else " " ).mkString
        println(f"|$rowStr|")
    }

    println(horizontalLine)
  }
}
