import collection.mutable
import collection.parallel.immutable.ParVector

/**
 * Created by Mitchell Vanderhoeff
 * Date: 2012-11-18
 * Time: 5:49 PM
 */

case class Cell(x:Int, y:Int, alive:Boolean = false) {
  def dead = !alive

  def isNeighbourWith(other:Cell):Boolean = {
    math.abs(other.x - this.x) + math.abs(other.y - this.x) < 2
  }
}

class World(val width:Int = 10, val height:Int = 10) {
  val EMPTY_CELLS:ParVector[Cell] = {
    ParVector.tabulate(width * height) {
      (index: Int) =>
        val x = index % width
        val y = math.floor(index / height).toInt
        Cell(x, y)
    }
  }

  var cells:ParVector[Cell] = EMPTY_CELLS

  var shouldLive:((Cell, ParVector[Cell]) => Boolean) = {
    (cell:Cell, neighbours:ParVector[Cell]) =>
      neighbours.length >= 3 && neighbours.length <= 5
  }

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
    cells = cells.map { (cell: Cell) =>
        val alive = shouldLive(cell, findNeighbours(cell))
        Cell(cell.x, cell.y, alive)
    }
  }

  def display() {

  }
}
