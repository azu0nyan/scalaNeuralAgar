package NNAgar

import NNAgar.game.{ControllablePlayer, GameInstance}
import NNAgar.game.GameModel.Game
import NNAgar.neural.{GeneticSorter, NeuralPlayer}

import java.awt.Graphics2D
import java.awt.image.BufferedImage
import javax.swing.{JFrame, WindowConstants}
import scala.util.Try

object Main {




  def main(args: Array[String]): Unit = {

    val sorter = new GeneticSorter()
    sorter.init()

    var cp : ControllablePlayer = null
    val window = new GameWindow(preDraw = {
      sorter.tick()
    }, draw = sorter.gameInstance.draw)

    cp = new ControllablePlayer(sorter.gameInstance, window.jf)

    window.startLoopSeparateThread()

  }




}