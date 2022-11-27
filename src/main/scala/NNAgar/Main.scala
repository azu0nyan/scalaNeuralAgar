package NNAgar

import NNAgar.game.{ControllablePlayer, GameInstance}
import NNAgar.game.GameModel.Game
import NNAgar.neural.{ConcurrentGeneticSorter, NeuralPlayer}

import java.awt.Graphics2D
import java.awt.event.{KeyEvent, KeyListener, MouseEvent, MouseListener}
import java.awt.image.BufferedImage
import javax.swing.{JFrame, WindowConstants}
import scala.util.Try

object Main {


  def main(args: Array[String]): Unit = {

    val sorter = new ConcurrentGeneticSorter()
    sorter.init()

    var cp: ControllablePlayer = null
    val window = new GameWindow(preDraw = {
      sorter.tick()
    }, draw = sorter.draw)

    window.jf.addKeyListener(new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = {}
      override def keyPressed(e: KeyEvent): Unit = {}
      override def keyReleased(e: KeyEvent): Unit = e.getKeyCode match
        case KeyEvent.VK_G =>
          sorter.drawGame = !sorter.drawGame
        case KeyEvent.VK_9 =>
          sorter.sleep = math.max(0, sorter.sleep - 10)
          window.jf.setTitle(s"Neural agar s: ${sorter.sleep}")
        case KeyEvent.VK_0 =>
          sorter.sleep = sorter.sleep match
            case 0 => 1
            case 1 => 10
            case x => x + 10
          window.jf.setTitle(s"Neural agar s: ${sorter.sleep}")
        case KeyEvent.VK_SPACE =>
          sorter.pause = !sorter.pause
        case _ =>
          
    })
    window.jf.addMouseListener(new MouseListener {
      override def mouseClicked(e: MouseEvent): Unit = {
        sorter.click(e.getX, e.getY)  
      }
      override def mousePressed(e: MouseEvent): Unit = {}
      override def mouseReleased(e: MouseEvent): Unit = {}
      override def mouseEntered(e: MouseEvent): Unit = {}
      override def mouseExited(e: MouseEvent): Unit = {}
    })

    //    cp = new ControllablePlayer(sorter.gameInstance, window.jf)

    window.startLoopSeparateThread()

  }


}