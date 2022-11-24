package NNAgar.game

import NNAgar.game.GameModel.*

import java.awt.{Color, Graphics2D}

class GameInstance(p: GameParams = GameParams()) {

  var g: Game = Game(params = p)


  def spawnPlayer(): Int = {
    val np = Player(g.deadPlayers.size + g.alivePlayers.size + 1, Helpers.randomInArea(g.params.area), V2(0, 0), g.params.initialSize, g.tick)
    g = g.copy(alivePlayers = g.alivePlayers :+ np)

    println(s"Player ${np.id} spawned at ${np.pos}.")
    np.id
  }

  def spawnFood(): Unit = {
    g = g.copy(food = g.food :+ Helpers.randomInArea(g.params.area))
  }

  def setMoveDirection(dir: V2, playerId: Int): Unit = {
    g.alivePlayers.indexWhere(_.id == playerId) match
      case -1 =>
      case pId =>
        val np = g.alivePlayers(pId).copy(dir = dir)
        g = g.copy(alivePlayers = g.alivePlayers.updated(pId, np))
  }

  def player(id: Int): Player = g.alivePlayers.find(_.id == id).getOrElse(g.deadPlayers.find(_.id == id).get)

  def update(): Unit = {
    println(s"Tick ${g.tick} players alive: ${g.alivePlayers.size}")
    val foodLeft = g.food.filter(f => !g.alivePlayers.exists(_.contains(f)))
    val af = for (p <- g.alivePlayers) yield
      p.copy(size = p.size + g.params.sizePerFood * g.food.count(p.contains))

    val (newAlive, newDead) = af
      .map { p =>
        val posDelta = p.dir * g.params.tickTime * g.params.speed(p.size)
        p.copy(pos = p.pos + posDelta,
          distanceTraveled = p.distanceTraveled + posDelta.length)
      }.sortBy(-_.size)
      .foldLeft((Seq[Player](), Seq[Player]())) {
        case ((alive, dead), canBeEaten) => alive.find(a => a.intersects(canBeEaten)) match
          case Some(eater) =>
            val eaterId = alive.indexOf(eater)
            val nA = alive.updated(eaterId, eater.copy(size = eater.size + canBeEaten.size))

            (nA, dead :+ canBeEaten.copy(deadAt = Some(g.tick)))
          case None => (alive :+ canBeEaten, dead)
      }

    g = g.copy(alivePlayers = newAlive,
      deadPlayers = g.deadPlayers ++ newDead,
      tick = g.tick + 1,
      food = foodLeft)
  }


  def draw(gr: Graphics2D): Unit = {
    gr.setColor(Color.GREEN)
    for (f <- g.food) gr.fillOval(f.x.toInt - 5, f.y.toInt - 5, 10, 10)


    gr.setColor(Color.RED)
    for (p <- g.alivePlayers) {
      gr.fillOval(p.pos.x.toInt - p.rad.ceil.toInt / 2, p.pos.y.toInt - p.rad.ceil.toInt / 2,
        p.rad.ceil.toInt, p.rad.ceil.toInt)


    }

  }
}
