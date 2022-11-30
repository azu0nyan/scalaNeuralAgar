package neural

import NNAgar.game.GameModel.Obstacle
import NNAgar.game.{Helpers, V2}
import org.scalatest.funsuite.AnyFunSuite

class ObstacleTest extends AnyFunSuite{

  test("seg"){

    assert(Helpers.segmentIntersection(V2(-100, -100), V2(-100, 100), V2(-200, -200), V2(200, 200))== Some(V2(-100, -100)))
  }

  test("seg2"){

    assert(Helpers.segmentIntersection(V2(100, -100), V2(100, 100), V2(200, 200), V2(100, 100))== Some(V2(100, 100)))
    assert(Helpers.segmentIntersection(V2(100, 100), V2(-100, 100), V2(200, 200), V2(100, 100))== Some(V2(100, 100)))
    assert(Helpers.segmentIntersection(V2(100, 100), V2(100, -100), V2(200, 200), V2(100, 100))== Some(V2(100, 100)))

  }

  test("collision"){
    val o = Obstacle(V2(-100, -100), V2(100, 100))

    assert(o.intersection(V2(-200, -200), V2(200, 200))== Some(V2(-100, -100)) )
    assert(o.intersection(V2(200, 200), V2(-200, -200)).get ~= V2(100, 100) )
    assert(o.intersection(V2(200, 200), V2(100, 100)) == Some(V2(100, 100)) )

    assert(o.intersection(V2(-200, -100), V2(200, -100)).get ~= V2(-100, -100) )
  }
}
