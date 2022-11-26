package NNAgar.game

import NNAgar.game.GameModel.{Game, Player}

object GameToNeuralOps {


  val visionSectors = 8

  val maxVisionDistance: Double = 300
  val maxSize: Double = 1000

  def playerVision(g: Game, pId: Int): IndexedSeq[Double] = {
    val p = g.player(pId)
    val ownPos = p.pos / g.params.area

    val sectorsData:Seq[Seq[Double]] = for(i <- 0 until visionSectors) yield {
      val sectorBegin = math.Pi * i / visionSectors
      val sectorEnd = math.Pi * (i + 1) / visionSectors

      val foodDist = g.food.filter{f =>
        val dist = p.pos - f
        val a = dist.angleToOx
        dist.length < maxVisionDistance && sectorBegin <= a && a < sectorEnd
      }.map(f => (p.pos - f).length / maxVisionDistance).minOption.getOrElse(1.0)

      val (enemyDist, enemySize) = g.alivePlayers.filter(_.id != pId).filter(enemy =>
        val dist = p.pos - enemy.pos
        val angle = dist.angleToOx
        dist.length < maxVisionDistance && sectorBegin <= angle && angle < sectorEnd
      ).map(e => ((p.pos - e.pos).length / maxVisionDistance, e.size)).minByOption(_._1).getOrElse((1d, 0d))

      Seq(foodDist, enemyDist, enemySize)
    }

    val res = IndexedSeq(
      p.size / maxSize,
      ownPos.x,
      ownPos.y) ++ (sectorsData.flatten.toIndexedSeq)
  //  println(res)
    res
  }

  def fitnessFunction(g: Game, pId: Int): Double = {
    val player = g.player(pId)
    val travelCoeff = player.distanceTraveled / g.params.speed(g.params.initialSize)
    math.pow(player.size / 100,  1.4) + 20 * math.pow(travelCoeff, 1.2d) / (player.aliveSec(g) + 1) //+ player.eatenFood * 20 + math.pow(player.eatenEnemy, 1.3d)
  }

  def playerControl(gi: GameInstance, pId: Int, act: IndexedSeq[Double]): Unit = {
    val dir = V2(act(0) - act(1), act(2) - act(3)) .capLength(1.0)
   // println(act.toString() + " " + dir)
//    val dir = gi.player(pId).dir.rotate(act(0) - act(1))
    gi.setMoveDirection(dir, pId)
  }
}
