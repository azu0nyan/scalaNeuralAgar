package NNAgar.game

import scala.util.Random

object Helpers {


  val eps:Double = 0.000001

  def randomInArea(area: V2, seed:Option[Int] = None): V2 = {
    val r = seed match
      case Some(value) => new Random(value)
      case None => new Random()
      
    V2(r.nextDouble() * area.x, r.nextDouble() * area.y)    
  }

  def segmentIntersection(s1:V2, e1: V2, s2:V2, e2:V2) :Option[V2]= {
    val dir1 = e1 - s1
    val dir2 = e2 - s2
    /*
     s1 + dir1 * u == s2 + dir2 * v

     dir1 * u - dir2 * v == s2 - s1

     dir1_x * u - dir2_x * v == s2_x - s1_x
     dir1_y * u - dir2_y * v == s2_y - s1_y
     */
    val det = dir1.x * (-dir2.y) - (-dir2.x) * dir1.y
    if(det != 0){
      val detX = (s2.x - s1.x) * (- dir2.y) - (-dir2.x) *(s2.y - s1.y)
      val detY = dir1.x * (s2.y - s1.y)  - (s2.x - s1.x) * dir1.y
      val u = detX / det
      val v = detY / det
      if(0-eps <= u && u <= 1 + eps && 0 -eps <= v && v <= 1 + eps)
        Some(s1 + dir1 * u)
      else None
    } else None


  }

}
