package NNAgar.game

import java.awt.{Color, Graphics2D}
import scala.util.Random

object GameModel {

  case class GameParams(initialSize: Double = 60d,
                        tickTime: Double = 1.0 / 60.0,
                        sizePerFood: Double = 30.0,
                        foodPerTick: Double = 0.4,
                        area: V2 = V2(1000, 1000),
                        dSizePerTick: Double = 0.3,
                        speed: Double => Double = size => 400 - 3 * math.sqrt(size),
                        seed: Int = new Random().nextInt())

  case class Player(id: Int,
                    pos: V2, dir: V2,
                    size: Double,
                    spawnedAt: Int = 0,
                    deadAt: Option[Int] = None,
                    eatenEnemy: Double = 0,
                    eatenFood: Double = 0,
                    distanceTraveled: Double = 0) {

    def rad: Double = math.sqrt(size)

    def contains(v: V2): Boolean = (pos - v).length < rad

    def intersects(ot: Player): Boolean = (pos - ot.pos).length < rad

    def alive: Boolean = deadAt.isEmpty
    
    def aliveSec(g: Game): Double = (deadAt.getOrElse(g.tick) - spawnedAt) * g.params.tickTime


  }

  case class Game(params: GameParams = GameParams(),
                  alivePlayers: Seq[Player] = Seq(),
                  food: Seq[V2] = Seq(),
                  tick: Int = 0,
                  deadPlayers: Seq[Player] = Seq()){

    def player(id:Int): Player = alivePlayers.find(_.id == id).getOrElse(deadPlayers.find(_.id == id).get)
  }




}
