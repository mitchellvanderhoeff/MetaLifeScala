import collection.parallel.immutable.ParVector

/**
 * Created by Mitchell Vanderhoeff
 * Date: 2012-11-18
 * Time: 9:35 PM
 */

val world = new World(width = 40, height = 15, lifeRules =
  (cell:Cell, neighbours:List[Cell]) => {
    if (neighbours.exists(cell.isSW(_)))
      LifeRules.live
    else if (cell.dead && neighbours.length == 1)
      LifeRules.live
    else if (cell.alive && neighbours.length >= 3)
      LifeRules.die
    else
      LifeRules.doNothing
  }
)

//world.randomize(ratio = 0.05)
world.spawnCellsAt((0, 0))

world.run(times = 10)

