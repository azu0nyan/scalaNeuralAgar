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

  def player(id: Int): Player = g.player(id)

  def tick(): Unit = {
    //println(s"Tick ${g.tick} players alive: ${g.alivePlayers.size}")
    val foodLeft = g.food.filter(f => !g.alivePlayers.exists(_.contains(f)))
    val af = for (p <- g.alivePlayers) yield
      p.copy(size = p.size + g.params.sizePerFood * g.food.count(p.contains))

    val (newAlive, newDead) = af
      .map { p =>
        val tryPos = p.pos  + p.dir * g.params.tickTime * g.params.speed(p.size)
        val newPos = V2(math.max(0, math.min(g.params.area.x, tryPos.x)), math.max(0, math.min(g.params.area.y, tryPos.y)))

        p.copy(pos = newPos,
          distanceTraveled = p.distanceTraveled + (p.pos - newPos).length)
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

    val tFBegin = g.tick * g.params.foodPerTick
    val tFEnd = (g.tick + 1) * g.params.foodPerTick
    for (_ <- tFBegin.ceil.toInt until tFEnd.ceil.toInt) spawnFood()
  }


  def draw(gr: Graphics2D): Unit = {
    gr.setColor(Color.BLACK)
    gr.fillRect(0, 0, g.params.area.x.toInt, g.params.area.y.toInt)


    for (f <- g.food) {

      gr.setColor(new Color(0, 255, 0, 70))
      gr.fillOval(f.x.toInt - 8, f.y.toInt - 8, 16, 16)
      gr.setColor(Color.GREEN)
      gr.fillOval(f.x.toInt - 5, f.y.toInt - 5, 10, 10)
    }


    for (p <- g.alivePlayers) {
      gr.setColor(new Color(255, 200, 200, 30))
      gr.fillOval(
        p.pos.x.toInt - p.rad.ceil.toInt - 3,
        p.pos.y.toInt - p.rad.ceil.toInt - 3,
        p.rad.ceil.toInt * 2 + 6, p.rad.ceil.toInt * 2 + 6)

      gr.setColor(new Color(255, 0, 0))
      gr.fillOval(p.pos.x.toInt - p.rad.ceil.toInt, p.pos.y.toInt - p.rad.ceil.toInt,
        p.rad.ceil.toInt * 2, p.rad.ceil.toInt * 2)


    }

  }
}
