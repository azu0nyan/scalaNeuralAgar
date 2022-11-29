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
    var window:GameWindow = null
    window = new GameWindow(preDraw = {
      sorter.tick()
      updateTitle()
    }, draw = sorter.draw)

    def updateTitle(): Unit = {
      window.jf.setTitle(s"Neural agar s: ${sorter.sleep} t: ${sorter.params.threads} gen: ${sorter.waveId} ${if (sorter.overrideRoundTicks.nonEmpty) s"r: ${sorter.overrideRoundTicks}" else ""}")
    }


    window.jf.addKeyListener(new KeyListener {


      override def keyTyped(e: KeyEvent): Unit = {}
      override def keyPressed(e: KeyEvent): Unit = {
        e.getKeyCode match
          case KeyEvent.VK_I =>
            sorter.overrideRoundTicks = sorter.overrideRoundTicks.map(_ + 100).orElse(Some(5000))
          case KeyEvent.VK_U =>
            sorter.overrideRoundTicks = sorter.overrideRoundTicks.map(_ - 100).orElse(Some(5000))
          case KeyEvent.VK_Y =>
            sorter.overrideRoundTicks = None

          case KeyEvent.VK_P =>
            sorter.pauseOnNewWave = !sorter.pauseOnNewWave

          case KeyEvent.VK_G =>
            sorter.drawGame = !sorter.drawGame
          case KeyEvent.VK_OPEN_BRACKET =>
            sorter.sleep = math.max(0, sorter.sleep - 10)

          case KeyEvent.VK_CLOSE_BRACKET =>
            sorter.sleep = sorter.sleep match
              case 0 => 1
              case 1 => 10
              case x => x + 10
          case KeyEvent.VK_SPACE =>
            sorter.pause = !sorter.pause
          case KeyEvent.VK_BACK_QUOTE => sorter.showFieldId = -1
          case KeyEvent.VK_1 => sorter.showFieldId = 0
          case KeyEvent.VK_2 => sorter.showFieldId = 1
          case KeyEvent.VK_3 => sorter.showFieldId = 2
          case KeyEvent.VK_4 => sorter.showFieldId = 3
          case KeyEvent.VK_5 => sorter.showFieldId = 5
          case KeyEvent.VK_6 => sorter.showFieldId = 6
          case KeyEvent.VK_7 => sorter.showFieldId = 7
          case KeyEvent.VK_8 => sorter.showFieldId = 8
          case KeyEvent.VK_9 => sorter.showFieldId = 9
          case KeyEvent.VK_0 => sorter.showFieldId = 10
          case _ =>
      }

      override def keyReleased(e: KeyEvent): Unit = {
      }

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