package NNAgar.game

import scala.util.Random

object Helpers {
  def randomInArea(area: V2, seed:Option[Int] = None): V2 = {
    val r = seed match
      case Some(value) => new Random(value)
      case None => new Random()
      
    V2(r.nextDouble() * area.x, r.nextDouble() * area.y)    
  } 

}
