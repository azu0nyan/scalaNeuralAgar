package NNAgar.game

import NNAgar.game.GameModel.{Game, Player}

import java.awt.Graphics2D
import scala.collection.immutable.IndexedSeq

object GameToNeuralOps {


  val visionSectors = 8

  val visionSize = visionSectors * 4 + 3
  val maxVisionDistance: Double = 300
  val maxSize: Double = 1000

  def playerVision(g: Game, pId: Int): IndexedSeq[Double] =
    playerVisionWithDrawing(g, pId, None, 0, 0, 1.0)

  def playerVisionWithDrawing(g: Game, pId: Int, grOpt: Option[Graphics2D],
                   dx: Int = 0, dy: Int = 0, scale: Double = 1.0): IndexedSeq[Double] = {
    val p = g.player(pId)
    val ownPos = p.pos / g.params.area


    val pgx = dx + p.pos.x * scale
    val pgy = dy + p.pos.y * scale

    val sectorsData:Seq[Seq[Double]] = for(i <- 0 until visionSectors) yield {

      val sectorBegin = math.Pi * 2d * i / visionSectors
      val sectorEnd = math.Pi * 2d * (i + 1) / visionSectors

      //draw
      for(gr <- grOpt) {
        val rayEndXX = pgx + maxVisionDistance * scale * math.cos(sectorBegin)
        val rayEndXY = pgy + maxVisionDistance * scale * math.sin(sectorBegin)
        gr.drawLine(pgx.toInt, pgy.toInt, rayEndXX.toInt, rayEndXY.toInt)
      }

      val foodDist = g.food.filter{f =>
        val dist = p.pos - f
        val a = dist.angleToOx
        dist.length < maxVisionDistance && sectorBegin <= a && a < sectorEnd
      }.map(f => (p.pos - f).length / maxVisionDistance).minOption.getOrElse(1.0)

      val foodSize = g.food.count { f =>
        val dist = p.pos - f
        val a = dist.angleToOx
        dist.length < maxVisionDistance && sectorBegin <= a && a < sectorEnd
      }

      val (enemyDist, enemySize) = g.alivePlayers.filter(_.id != pId).filter(enemy =>
        val dist = p.pos - enemy.pos
        val angle = dist.angleToOx
        dist.length < maxVisionDistance && sectorBegin <= angle && angle < sectorEnd
      ).map(e => ((p.pos - e.pos).length / maxVisionDistance, e.size / maxSize)).minByOption(_._1).getOrElse((1d, 0d))

      Seq(foodDist, math.min(1d, foodSize / 32d),enemyDist, enemySize / maxSize)
    }

    val res =  (sectorsData.flatten.toIndexedSeq) ++ IndexedSeq(
      p.size / maxSize,
      ownPos.x,
      ownPos.y)
  //  println(res)
    res
  }

  def fitnessFunction(g: Game, pId: Int): Double = {
    val player = g.player(pId)
//    val travelCoeff = 1.3 * player.distanceTraveled / g.params.speed(g.params.initialSize)
// +
     /*player.distanceTraveled / 1000d  +*/
    player.eatenFood * 40 + player.eatenEnemy * 50 + 30 * player.aliveSec(g) + 2 * player.size +
      (if(player.deadAt.nonEmpty) -2000 else 0)
  }

  def playerControl(gi: GameInstance, pId: Int, act: IndexedSeq[Double]): Unit = {
    val dir = V2(act(0) - act(1), act(2) - act(3)) .capLength(1.0)
   // println(act.toString() + " " + dir)
//    val dir = gi.player(pId).dir.rotate(act(0) - act(1))
    gi.setMoveDirection(dir, pId)
  }
}
