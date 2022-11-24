package NNAgar

import NNAgar.game.{ControllablePlayer, GameInstance}
import NNAgar.game.GameModel.Game

import java.awt.Graphics2D
import java.awt.image.BufferedImage
import javax.swing.{JFrame, WindowConstants}

object Main {




  def main(args: Array[String]): Unit = {
    val g = new GameInstance()
    val window = new GameWindow(preDraw = g.update(), draw = g.draw)
    new ControllablePlayer(g, window.jf)

    window.startLoopSeparateThread()

  }




}