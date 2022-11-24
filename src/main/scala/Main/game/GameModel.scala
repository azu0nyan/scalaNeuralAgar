package Main.game

import java.awt.{Color, Graphics2D}

object GameModel {

  case class GameParams(initialSize: Double = 10d,
                        tickTime: Double = 1.0 / 60.0,
                        sizePerFood: Double = 1.0)

  case class Player(id: Int,
                    pos: V2, dir: V2,
                    size: Double,
                    spawnedAt: Int = 0,
                    deadAt: Option[Int] = None,
                    eatenEnemy: Double = 0,
                    eatenFood: Double = 0) {

    def rad: Double = math.sqrt(size)

    def contains(v: V2): Boolean = (pos - v).length < rad

    def intersects(ot: Player): Boolean = (pos - ot.pos).length < rad
  }

  case class Game(params: GameParams,
                  alivePlayers: Seq[Player],
                  food: Seq[V2],
                  tick: Int = 0,
                  deadPlayers: Seq[Player] = Seq())

  def spawnPlayer(g: Game, at: V2): (Game, Int) = {
    val np = Player(g.deadPlayers.size + g.alivePlayers.size, at, V2(0, 0), g.params.initialSize, g.tick)
    val ng = g.copy(alivePlayers = g.alivePlayers :+ np)
    (ng, np.id)
  }


  def setMoveDirection(g: Game, dir: V2, id: Int): Game = {
    val np = g.alivePlayers(id).copy(dir = dir)
    g.copy(alivePlayers = g.alivePlayers.updated(id, np))
  }

  def update(g: Game): Game = {
    val foodLeft = g.food.filter(f => !g.alivePlayers.exists(_.contains(f)))
    var af = for (p <- g.alivePlayers) yield
      p.copy(size = p.size + g.params.sizePerFood * g.food.count(p.contains))

    val (newAlive, newDead) = af
      .map(p => p.copy(pos = p.pos + p.dir * g.params.tickTime))
      .sortBy(-_.size)
      .foldLeft((Seq[Player](), Seq[Player]())) {
        case ((alive, dead), canBeEaten) => alive.find(a => a.intersects(canBeEaten)) match
          case Some(eater) =>
            val eaterId = alive.indexOf(eater)
            val nA = alive.updated(eaterId, eater.copy(size = alive.size + canBeEaten.size))

            (nA, dead :+ canBeEaten.copy(deadAt = Some(g.tick)))
          case None => (alive :+ canBeEaten, dead)
      }

    g.copy(alivePlayers = newAlive,
      deadPlayers = g.deadPlayers ++ newDead,
      tick = g.tick + 1,
      food = foodLeft)
  }


  def draw(game: Game, g: Graphics2D): Unit = {
    g.setColor(Color.GREEN)
    for (f <- game.food) g.fillOval(f.x.toInt - 5, f.x.toInt - 5, 10, 10)
    g.setColor(Color.RED)
    for (p <- game.alivePlayers) g.fillOval(
      p.pos.x.toInt - p.rad.ceil.toInt / 2,
      p.pos.y.toInt - p.rad.ceil.toInt / 2,
      p.rad.ceil.toInt, p.rad.ceil.toInt)


  }


}
