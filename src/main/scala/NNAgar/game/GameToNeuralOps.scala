package NNAgar.game

import NNAgar.game.GameModel.{Game, Player}

import java.awt.{BasicStroke, Color, Graphics2D}
import scala.collection.immutable.IndexedSeq

object GameToNeuralOps {


  val visionSectors = 8

  val visionSize = visionSectors * 3 + 1
  val baseVisionDistance = 115
  val maxVisionDistance: Player =>  Double =  p => p.rad + baseVisionDistance
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

      val sectorMiddle = (a + math.Pi * 2d * (i ) / visionSectors) % (math.Pi * 2d)

      val sectorEnd = (a + math.Pi * 2d * (i + 0.5) / visionSectors) % (math.Pi * 2d)

      def inAngle(b:Double, e:Double, x:Double): Boolean =
        if(b < e) b <= x && x < e
        else b <= x && x > e

      //draw
      val visionDistance = maxVisionDistance(p)


      val visionRay = V2(visionDistance, 0).rotate(sectorMiddle)
      val collision = (g.obstacles).flatMap(_.intersection(p.pos, p.pos + visionRay)).minByOption(c => (p.pos - c).length)
      val collisionDist = collision.map(c => (p.pos - c).length)
      //draw
      for(V2(x, y) <- collision; gr <- grOpt){
        var rx =  dx + x * scale
        var ry =  dy + y * scale
        gr.setStroke(new BasicStroke(0.7f))
        gr.setColor(new Color(255, 255, 255, 255))
        gr.drawLine(pgx.toInt, pgy.toInt, rx.toInt, ry.toInt)
      }


      for(gr <- grOpt) {
        val rayEndXX = pgx + visionDistance * scale * math.cos(sectorBegin)
        val rayEndXY = pgy + visionDistance * scale * math.sin(sectorBegin)
        gr.setStroke(new BasicStroke(0.7f))
        gr.setColor(new Color(255, 20, 200, 110))
        gr.drawLine(pgx.toInt, pgy.toInt, rayEndXX.toInt, rayEndXY.toInt)
      }

      val foodDist = g.food.filter{f =>
        val dist = p.pos - f
        val a = dist.angleToOx
        dist.length < visionDistance && inAngle(sectorBegin, sectorEnd, a)
      }.map(f => ((p.pos - f).length - p.rad)/baseVisionDistance).minOption.getOrElse(1.0)

      //draw
      for(gr <- grOpt; f <- g.food.filter { f =>
        val dist = p.pos - f
        val a = dist.angleToOx
        dist.length < visionDistance && inAngle(sectorBegin, sectorEnd, a)
      }.minByOption(f => (p.pos - f).length )){
        gr.setStroke(new BasicStroke(0.7f))
        gr.setColor(new Color(0, 155, 0))
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
      ).map(e => (((p.pos - e.pos).length - p.rad)/ baseVisionDistance, e.size )).minByOption(_._1).getOrElse((1d, 0d))

      //draw
      for (gr <- grOpt; p <- g.alivePlayers.filter(_.id != pId).filter(enemy =>
        val dist = p.pos - enemy.pos
        val angle = dist.angleToOx
        dist.length < visionDistance && inAngle(sectorBegin, sectorEnd, angle)
      ).minByOption(e => ((p.pos - e.pos).length, e.size))) {
        gr.setStroke(new BasicStroke(0.7f))
        gr.setColor(new Color(255, 0, 0))
        val pX = dx + p.pos.x * scale
        val pY = dy + p.pos.y * scale
        gr.drawLine(pgx.toInt, pgy.toInt, pX.toInt, pY.toInt)
      }

      Seq(
        1 - collisionDist.map(c => (c - p.rad) / baseVisionDistance).getOrElse(1d),
//        1 - enemyDist,
//        enemySize / maxSize,
        1 - foodDist,
        math.min(1d, foodSize / 5d)
      )
    }

    val sd = sectorsData.flatten.toIndexedSeq

    val reordered = for(param <- Seq
    (Seq(0), 
      Seq(1, 2),
//      Seq(3, 4)
    );
        s <- 0 until visionSectors;
        p <- param
        ) yield sd(3 * s + p)

    reordered.toIndexedSeq ++ IndexedSeq(p.size / maxSize)
//    val res =  (sectorsData.flatten.toIndexedSeq) ++ IndexedSeq(
//      p.size / maxSize    )
//    res.s
  //  println(res)
//    res
  }

  def fitnessFunction(g: Game, pId: Int): Double = {
    val player = g.player(pId)
//    val travelCoeff = 1.3 * player.distanceTraveled / g.params.speed(g.params.initialSize)
// +
     /*player.distanceTraveled / 1000d  +*/
    100 * player.eatenFood / math.max(1, player.aliveSec(g))  + math.pow(player.eatenEnemy, 1.3)   + player.size //+
//      (if(player.deadAt.nonEmpty) -2000 else 0)
  }

  def playerControl(gi: GameInstance, pId: Int, act: IndexedSeq[Double]): Unit = {
//    val dir = V2(act(0) - act(1), 1) .capLength(1.0)
    val dir = V2(act(0) - act(1), act(2) - act(3)) .capLength(1.0)
   // println(act.toString() + " " + dir)
//    val dir = gi.player(pId).dir.rotate(act(0) - act(1))
    gi.setMoveDirection(dir, pId)
  }
}
