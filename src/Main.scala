import collection.parallel.immutable.ParVector

/**
 * Created by Mitchell Vanderhoeff
 * Date: 2012-11-18
 * Time: 9:35 PM
 */

val world = new World(width = 20, height = 10, lifeRules = {
  (cell:Cell, neighbours:ParVector[Cell]) =>
    if (cell.alive && neighbours.length >= 6) LifeRules.die
    else if (cell.dead && neighbours.length == 0) LifeRules.live
    else LifeRules.doNothing
})

world.randomize(ratio = 0.35)
world.run(times = 20)

