package NNAgar

import NNAgar.game.{ControllablePlayer, GameInstance}
import NNAgar.game.GameModel.Game
import NNAgar.neural.NeuralPlayer

import java.awt.Graphics2D
import java.awt.image.BufferedImage
import javax.swing.{JFrame, WindowConstants}
import scala.util.Try

object Main {




  def main(args: Array[String]): Unit = {
    val g = new GameInstance()
//    val nps = for(i <- 0 until 20) yield  new NeuralPlayer(g)
//    for(i <- 0 until 100) g.spawnFood()
//
    var cp : ControllablePlayer = null
    val window = new GameWindow(preDraw = {
//      nps.foreach(_.tick())
      g.update()
      println(Try(g.player(cp.id).size))
    }, draw = g.draw)
    cp = new ControllablePlayer(g, window.jf)

    window.startLoopSeparateThread()

  }




}