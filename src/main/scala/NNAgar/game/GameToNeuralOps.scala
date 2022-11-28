package NNAgar.game

import NNAgar.game.GameModel.{Game, Player}

import java.awt.{Color, Graphics2D}
import scala.collection.immutable.IndexedSeq

object GameToNeuralOps {


  val visionSectors = 7

  val visionSize = visionSectors * 4 + 5
  val maxVisionDistance: Player =>  Double =  p => p.rad + 115
  val maxSize: Double = 1000

  def playerVision(g: Game, pId: Int): IndexedSeq[Double] =
    playerVisionWithDrawing(g, pId, None, 0, 0, 1.0)

  def playerVisionWithDrawing(g: Game, pId: Int, grOpt: Option[Graphics2D],
                   dx: Int = 0, dy: Int = 0, scale: Double = 1.0): IndexedSeq[Double] = {
    val p = g.player(pId)
    val ownPos = p.pos / g.params.area


    val pgx = dx + p.pos.x * scale
    val pgy = dy + p.pos.y * scale

    val a = p.lookDir.angleToOx
    val sectorsData:Seq[Seq[Double]] = for(i <- 0 until visionSectors) yield {

      val sectorBegin = (a + math.Pi * 2d * (i - 0.5) / visionSectors) % (math.Pi * 2d)
      val sectorEnd = (a + math.Pi * 2d * (i + 0.5) / visionSectors) % (math.Pi * 2d)

      def inAngle(b:Double, e:Double, x:Double): Boolean =
        if(b < e) b <= x && x < e
        else b <= x && x > e

      //draw
      val visionDistance = maxVisionDistance(p)
      for(gr <- grOpt) {
        val rayEndXX = pgx + visionDistance * scale * math.cos(sectorBegin)
        val rayEndXY = pgy + visionDistance * scale * math.sin(sectorBegin)
        gr.setColor(new Color(255, 20, 200))
        gr.drawLine(pgx.toInt, pgy.toInt, rayEndXX.toInt, rayEndXY.toInt)
      }

      val foodDist = g.food.filter{f =>
        val dist = p.pos - f
        val a = dist.angleToOx
        dist.length < visionDistance && inAngle(sectorBegin, sectorEnd, a)
      }.map(f => (p.pos - f).length / visionDistance).minOption.getOrElse(1.0)

      //draw
      for(gr <- grOpt; f <- g.food.filter { f =>
        val dist = p.pos - f
        val a = dist.angleToOx
        dist.length < visionDistance && inAngle(sectorBegin, sectorEnd, a)
      }.minByOption(f => (p.pos - f).length )){
        gr.setColor(new Color(0, 255, 0))
        val fX = dx + f.x * scale
        val fY = dy + f.y * scale
        gr.drawLine(pgx.toInt, pgy.toInt, fX.toInt, fY.toInt)
      }

      val foodSize = g.food.count { f =>
        val dist = p.pos - f
        val a = dist.angleToOx
        dist.length < visionDistance && inAngle(sectorBegin, sectorEnd, a)
      }

      val (enemyDist, enemySize) = g.alivePlayers.filter(_.id != pId).filter(enemy =>
        val dist = p.pos - enemy.pos
        val angle = dist.angleToOx
        dist.length < visionDistance && inAngle(sectorBegin, sectorEnd, angle)
      ).map(e => ((p.pos - e.pos).length / visionDistance, e.size / maxSize)).minByOption(_._1).getOrElse((1d, 0d))

      //draw
      for (gr <- grOpt; p <- g.alivePlayers.filter(_.id != pId).filter(enemy =>
        val dist = p.pos - enemy.pos
        val angle = dist.angleToOx
        dist.length < visionDistance && inAngle(sectorBegin, sectorEnd, angle)
      ).minByOption(e => ((p.pos - e.pos).length / visionDistance, e.size / maxSize))) {
        gr.setColor(new Color(255, 0, 0))
        val pX = dx + p.pos.x * scale
        val pY = dy + p.pos.y * scale
        gr.drawLine(pgx.toInt, pgy.toInt, pX.toInt, pY.toInt)
      }

      Seq(foodDist, math.min(1d, foodSize / 32d),enemyDist, enemySize / maxSize)
    }

    val res =  (sectorsData.flatten.toIndexedSeq) ++ IndexedSeq(
      p.size / maxSize,
      ownPos.x,
      ownPos.y,
      1 - ownPos.x,
      1 - ownPos.y)
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
