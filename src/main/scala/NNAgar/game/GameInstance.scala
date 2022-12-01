package NNAgar.game

import NNAgar.game.GameModel.*

import java.awt.{BasicStroke, Color, Font, Graphics2D}
import scala.util.Random

class GameInstance(p: GameParams = GameParams()) {

  var gameData: Game = Game(params = p)

  spawnObstacles(p.initialObstacles, p.obstacleGridSize)
  for (i <- 0 until p.initialFood) spawnFood()


  def spawnObstacles(count: Int, gridSize: Int): Unit = {
    if (count > 0) {

      val freeCount = gridSize * gridSize - count
      val g: Array[Array[Boolean]] = Array.ofDim(gridSize, gridSize)

      val r = new Random()
      val gx = r.nextInt(gridSize)
      val gy = r.nextInt(gridSize)
      g(gx)(gy) = true
      var trys: Int = 0
      var spawned: Int = 1
      while (trys < 10000 && spawned < freeCount) {
        val gx = r.nextInt(gridSize)
        val gy = r.nextInt(gridSize)
        if (
          !g(gx)(gy) && (
            gx > 0 && g(gx - 1)(gy) ||
              gx < gridSize - 1 && g(gx + 1)(gy) ||
              gy > 0 && g(gx)(gy - 1) ||
              gy < gridSize - 1 && g(gx)(gy + 1))
        ) {
          g(gx)(gy) = true
          spawned += 1
        }


        trys += 1
      }
      for (i <- 0 until gridSize; j <- 0 until (gridSize)) {
        if (!g(i)(j)) {
          spawnObstacle(i, j, gridSize)
        }
      }
    }

  }

  def spawnObstacle(gx: Int, gy: Int, gridSize: Int): Unit = {
    val lr = Helpers.randomInArea(p.obstacleDelta) + p.obstacleMin
    //    val center = Helpers.randomInArea(gameData.params.area)
    val center = V2((gx + 0.5) / gridSize, (gy + 0.5) / gridSize) * gameData.params.area


    val min = center - lr * 0.5d
    val max = center + lr * 0.5d
    val cMin = V2(
      math.max(0d, math.min(gameData.params.area.x, min.x)),
      math.max(0d, math.min(gameData.params.area.y, min.y))
    )
    val cMax = V2(
      math.max(0d, math.min(gameData.params.area.x, max.x)),
      math.max(0d, math.min(gameData.params.area.y, max.y))
    )
    gameData = gameData.copy(obstacles = gameData.obstacles :+ Obstacle(cMin, cMax))
  }

  def spawnPlayer(): Int = {
    val np = Player(gameData.deadPlayers.size + gameData.alivePlayers.size + 1,
      randomPosition(), V2(0, 0),
      gameData.params.initialSize,
      gameData.tick,
      lookDir = new V2(1, 0).rotate(new Random().nextDouble() * Math.PI * 2d))
    gameData = gameData.copy(alivePlayers = gameData.alivePlayers :+ np)

    //    println(s"Player ${np.id} spawned at ${np.pos}.")
    np.id
  }

  def randomPosition(): V2 = {
    var pos = Helpers.randomInArea(gameData.params.area)
    var tr = 0
    while (tr < 1000000 && gameData.obstacles.exists(_.contains(pos)))
      pos = Helpers.randomInArea(gameData.params.area)
      tr += 1

    pos
  }

  def spawnFood(): Unit = {
    gameData = gameData.copy(food = gameData.food :+ randomPosition())
  }

  def setMoveDirection(dir: V2, playerId: Int): Unit = {
    gameData.alivePlayers.indexWhere(_.id == playerId) match
      case -1 =>
      case pId =>
        val np = gameData.alivePlayers(pId).copy(controlDir = dir)
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
        //        val tryPos = p.pos + p.dir * gameData.params.tickTime * gameData.params.speed(p.size)//x,y movement
        val rotationAngle = gameData.params.angleSpeedMax * gameData.params.tickTime * math.max(-1d, math.min(1d, p.controlDir.x))
        val newLookDir = p.lookDir.rotate(rotationAngle)

        val speedControl = math.max(-1d, math.min(1d, p.controlDir.y))
        val tryPosQ = p.pos + newLookDir * gameData.params.tickTime * gameData.params.speed(p.size) * (if (speedControl > 0) speedControl else speedControl * 0.70) //dir = lr, speed
        val tryObstaclePos = gameData.obstacles.flatMap(_.intersection(p.pos, tryPosQ)).minByOption(v => (v - p.pos).length) match
          case Some(value) => value + (p.pos - value) * 0.01///p.pos + (value - p.pos) * 0.99 //clamp toOuter
          case None => tryPosQ

        val newPos = V2(math.max(0, math.min(gameData.params.area.x, tryObstaclePos.x)), math.max(0, math.min(gameData.params.area.y, tryObstaclePos.y)))
        val dmgMovementDmg = (newPos - tryPosQ).length
        val hitWallDmg = gameData.obstacles.map(_.distance(p.pos)).map(dist => p.rad - dist).filter(_  > 0).sum

        p.copy(
          pos = newPos,
          distanceTraveled = p.distanceTraveled + (p.pos - newPos).length,
          size = math.max(0, p.size - dmgMovementDmg - hitWallDmg - gameData.params.dSizePerTick),
          lookDir = newLookDir)
      }.sortBy(-_.size)
      .foldLeft((Seq[Player](), Seq[Player]())) {
        case ((alive, dead), canBeEaten) => alive.find(a => a.intersects(canBeEaten)) match
          case Some(eater) =>
            val eaterId = alive.indexOf(eater)
            val nA = alive.updated(eaterId, eater.copy(size = eater.size + canBeEaten.size))

//            (nA, dead :+ canBeEaten.copy(deadAt = Some(gameData.tick), size = 0))
            ///////
            (alive :+ canBeEaten, dead)
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
    if (gameData.food.size < gameData.params.maxFood)
      for (_ <- tFBegin.ceil.toInt until tFEnd.ceil.toInt) spawnFood()
  }


  def draw(gr: Graphics2D, x: Double, y: Double, scale: Double, selectedPlayers: Seq[Int]): Unit = {
    gr.setColor(Color.BLACK)
    gr.fillRect(x.toInt, y.toInt, (scale * gameData.params.area.x).toInt, (scale * gameData.params.area.y).toInt)


    for (o <- gameData.obstacles) {
      gr.setStroke(new BasicStroke(2, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 10f, Array(1f, 1f), 0f))
      gr.setColor(new Color(70, 70, 70))
      gr.drawRect((x + o.min.x * scale).toInt, (y + o.min.y * scale).toInt, (o.width * scale).toInt, (o.height * scale).toInt)
    }

    gr.setStroke(new BasicStroke(1))
    for (f <- gameData.food) {
      gr.setColor(new Color(0, 255, 0, 70))
      gr.fillOval((x + f.x * scale - 3).toInt, (y + f.y * scale - 3).toInt, 6, 6)
      gr.setColor(Color.GREEN)
      gr.fillOval((x + f.x * scale - 2).toInt, (y + f.y * scale - 2).toInt, 4, 4)
    }

    for (p <- gameData.alivePlayers) {
      if (selectedPlayers.contains(p.id)) {
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

      //      if (y.toInt + (p.pos.y * scale - p.rad * scale).toInt <= 30)
      //        println()
      //      gr.setColor(new Color(255, 255, 255))
      //      gr.setFont(new Font("", Font.BOLD, 12))
      //      gr.drawString(p.id.toInt.toString + " " + p.size.toInt.toString, p.pos.x.toInt, p.pos.y.toInt)
      //

      if (selectedPlayers.contains(p.id)) {
        gr.setStroke(new BasicStroke(2f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
        gr.setColor(new Color(0, 100, 255))
        val vd = p.lookDir * gameData.params.speed(p.size) * scale * 0.2
        val vlr = p.lookDir.rotate(Math.PI / 2d) * gameData.params.speed(p.size) * scale * p.controlDir.x * 0.2
        val vSpeed = p.lookDir * gameData.params.speed(p.size) * scale * p.controlDir.y * 0.2

        val xPos = (x + p.pos.x * scale).toInt
        val yPos = (y + p.pos.y * scale).toInt
        gr.setColor(new Color(0, 100, 255))
        gr.drawLine(xPos, yPos, xPos + vd.x.toInt, yPos + vd.y.toInt)
        gr.setStroke(new BasicStroke(4f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
        gr.setColor(new Color(0, 255, 100))
        gr.drawLine(xPos, yPos, xPos + vSpeed.x.toInt, yPos + vSpeed.y.toInt)
        gr.setColor(new Color(222, 127, 20))
        gr.drawLine(xPos, yPos, xPos + vlr.x.toInt, yPos + vlr.y.toInt)
      }
    }

  }
}
