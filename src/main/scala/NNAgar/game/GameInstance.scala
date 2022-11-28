package NNAgar.game

import NNAgar.game.GameModel.*

import java.awt.{Color, Font, Graphics2D}

class GameInstance(p: GameParams = GameParams()) {

  var gameData: Game = Game(params = p)
  for (i <- 0 until p.initialFood) spawnFood()


  def spawnPlayer(): Int = {
    val np = Player(gameData.deadPlayers.size + gameData.alivePlayers.size + 1, Helpers.randomInArea(gameData.params.area), V2(0, 0), gameData.params.initialSize, gameData.tick)
    gameData = gameData.copy(alivePlayers = gameData.alivePlayers :+ np)

    //    println(s"Player ${np.id} spawned at ${np.pos}.")
    np.id
  }

  def spawnFood(): Unit = {
    gameData = gameData.copy(food = gameData.food :+ Helpers.randomInArea(gameData.params.area))
  }

  def setMoveDirection(dir: V2, playerId: Int): Unit = {
    gameData.alivePlayers.indexWhere(_.id == playerId) match
      case -1 =>
      case pId =>
        val np = gameData.alivePlayers(pId).copy(dir = dir)
        gameData = gameData.copy(alivePlayers = gameData.alivePlayers.updated(pId, np))
  }

  def player(id: Int): Player = gameData.player(id)

  def removePlayer(id: Int): Unit =
    gameData = gameData.copy(alivePlayers = gameData.alivePlayers.filter(_.id != id), deadPlayers = gameData.deadPlayers.filter(_.id != id))

  def tick(): Unit = {
    //println(s"Tick ${g.tick} players alive: ${g.alivePlayers.size}")
    val foodLeft = gameData.food.filter(f => !gameData.alivePlayers.exists(_.contains(f)))
    val af = for (p <- gameData.alivePlayers) yield {
      val foodEaten = gameData.food.count(p.contains)
      p.copy(size = p.size + gameData.params.sizePerFood * foodEaten, eatenFood = p.eatenFood + foodEaten)
    }

    val (newAliveT, newDeadT) = af
      .map { p =>
        val tryPos = p.pos + p.dir * gameData.params.tickTime * gameData.params.speed(p.size)
        val newPos = V2(math.max(0, math.min(gameData.params.area.x, tryPos.x)), math.max(0, math.min(gameData.params.area.y, tryPos.y)))
        val dmg = (newPos - tryPos).length

        p.copy(pos = newPos,
          distanceTraveled = p.distanceTraveled + (p.pos - newPos).length, size = math.max(0, p.size - dmg - gameData.params.dSizePerTick))
      }.sortBy(-_.size)
      .foldLeft((Seq[Player](), Seq[Player]())) {
        case ((alive, dead), canBeEaten) => alive.find(a => a.intersects(canBeEaten)) match
          case Some(eater) =>
            val eaterId = alive.indexOf(eater)
            val nA = alive.updated(eaterId, eater.copy(size = eater.size + canBeEaten.size))

            (nA, dead :+ canBeEaten.copy(deadAt = Some(gameData.tick), size = 0))
          case None => (alive :+ canBeEaten, dead)
      }
    val (newAlive, moreDead) = newAliveT.partition(p => p.size > 0)
    val newDead = newDeadT ++ moreDead.map(_.copy(deadAt = Some(gameData.tick), size = 0))

    gameData = gameData.copy(alivePlayers = newAlive,
      deadPlayers = gameData.deadPlayers ++ newDead,
      tick = gameData.tick + 1,
      food = foodLeft)

    val tFBegin = gameData.tick * gameData.params.foodPerTick
    val tFEnd = (gameData.tick + 1) * gameData.params.foodPerTick
    if(gameData.food.size < gameData.params.maxFood)
    for (_ <- tFBegin.ceil.toInt until tFEnd.ceil.toInt) spawnFood()
  }


  def draw(gr: Graphics2D, x: Double, y: Double, scale: Double, selectedPlayers:Seq[Int]): Unit = {
    gr.setColor(Color.BLACK)
    gr.fillRect(x.toInt, y.toInt, (scale * gameData.params.area.x).toInt, (scale * gameData.params.area.y).toInt)

    for (f <- gameData.food) {
      gr.setColor(new Color(0, 255, 0, 70))
      gr.fillOval((x + f.x * scale - 3).toInt, (y + f.y * scale - 3).toInt, 6, 6)
      gr.setColor(Color.GREEN)
      gr.fillOval((x + f.x * scale - 2).toInt, (y + f.y * scale - 2).toInt, 4, 4)
    }

    for (p <- gameData.alivePlayers) {
      if(selectedPlayers.contains(p.id)){
        gr.setColor(new Color(0, 255, 232))
      } else {
        gr.setColor(new Color(255, 200, 200, 30))
      }
      gr.fillOval(
        (x + p.pos.x * scale - p.rad * scale - 3).toInt,
        (y + p.pos.y * scale - p.rad * scale - 3).toInt,
        ((p.rad * scale).ceil * 2 + 6).toInt,
        ((p.rad * scale).ceil * 2 + 6).toInt)

      gr.setColor(new Color(255, 0, 0))
      gr.fillOval(
        x.toInt + (p.pos.x * scale - p.rad * scale).toInt,
        y.toInt + (p.pos.y * scale - p.rad * scale).toInt,
        ((p.rad * scale).ceil * 2).toInt,
        ((p.rad * scale).ceil * 2).toInt)

      if(y.toInt + (p.pos.y * scale - p.rad * scale).toInt <= 30)
        println()
//      gr.setColor(new Color(255, 255, 255))
//      gr.setFont(new Font("", Font.BOLD, 12))
//      gr.drawString(p.id.toInt.toString + " " + p.size.toInt.toString, p.pos.x.toInt, p.pos.y.toInt)
//
    }

  }
}
