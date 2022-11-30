package NNAgar.game

import java.awt.{Color, Graphics2D}
import scala.util.Random

object GameModel {

  case class GameParams(initialSize: Double = 40d,
                        tickTime: Double = 1.0 / 60.0,
                        sizePerFood: Double = 30.0,
                        initialFood: Int = 30,
                        maxFood: Int = 30,
                        foodPerTick: Double = 0.2,
                        area: V2 = V2(1000, 1000),
                        dSizePerTick: Double = 0.02,

                        initialObstacles: Int = 4,
                        obstacleMin:V2 = V2(64, 64),
                        obstacleMax:V2 = V2(64, 64),
                        obstacleGridSize: Int = 4,

                        angleSpeedMax: Double = 12 * Math.PI ,
                        speed: Double => Double = size => math.max(10, 400 - 400 * size / 1000d),
                        seed: Int = new Random().nextInt()){
    def obstacleDelta:V2 = obstacleMax - obstacleMin
  }

  case class Player(id: Int,
                    pos: V2, controlDir: V2,
                    size: Double,
                    spawnedAt: Int = 0,
                    deadAt: Option[Int] = None,
                    lookDir: V2 = V2(1, 0),
                    eatenEnemy: Double = 0,
                    eatenFood: Double = 0,
                    distanceTraveled: Double = 0) {

    def rad: Double = math.sqrt(size)

    def contains(v: V2): Boolean = (pos - v).length < rad

    def intersects(ot: Player): Boolean = (pos - ot.pos).length < rad

    def alive: Boolean = deadAt.isEmpty

    def aliveSec(g: Game): Double = (deadAt.getOrElse(g.tick) - spawnedAt) * g.params.tickTime


  }

  case class Obstacle(min:V2, max:V2) {
    def width: Double = max.x - min.x
    def height: Double = max.y - min.y

    def contains(v:V2):Boolean =
      min.x <= v.x && v.x <= max.x && min.y <= v.y && v.y <= max.y

    /**Returns closest intersection*/
    def intersection(origin:V2, end: V2):Option[V2] = {
      sides.flatMap{case (s, e) => Helpers.segmentIntersection(s,e, origin, end)}.minByOption(v => (v - origin).length)
    }


    val vertices:Seq[V2]= Seq(
      min,
      V2(min.x, max.y),
      max,
      V2(max.x, min.y)
    )

    val sides:Seq[(V2, V2)]=  (vertices :+ vertices.head).sliding(2).map{case Seq(a, b) => (a,b)}.toSeq

    def distance(p:V2):Double = {
      if(p.x <= min.x)
        if (p.y <= min.y) (min - p).length
        else if (p.y <= max.y) min.x - p.x
        else (V2(min.x, max.y) - p).length
      else if( p.x <= max.x)
        if (p.y <= min.y) min.y - p.y
        else if (p.y <= max.y) -1 //inside
        else p.y - max.y
      else
        if (p.y <= min.y) (V2(max.x, min.y) - p).length
        else if (p.y <= max.y) p.x - max.x
        else (max - p).length

    }

  }

  case class Game(params: GameParams = GameParams(),
                  obstacles: Seq[Obstacle] = Seq(),
                  alivePlayers: Seq[Player] = Seq(),
                  food: Seq[V2] = Seq(),
                  tick: Int = 0,
                  deadPlayers: Seq[Player] = Seq()){

    def player(id:Int): Player = alivePlayers.find(_.id == id).getOrElse(deadPlayers.find(_.id == id).get)
    def playerCount:Int = alivePlayers.size + deadPlayers.size

    val border:Seq[Obstacle] =
      Seq(
        Obstacle(V2(0, -100), V2(params.area.x, 0)),
        Obstacle(V2(-100, 0), V2(0, params.area.y)),
        Obstacle(V2(params.area.x, 0), V2(params.area.x + 100, params.area.y)),
        Obstacle(V2(0, params.area.y), V2(params.area.x, params.area.y + 100)),
      )
  }




}
