package NNAgar.game

import java.awt.event.{KeyEvent, KeyListener}
import javax.swing.JFrame
import scala.collection.mutable

class ControllablePlayer(gameInstance: GameInstance, jf:JFrame,
                         up: Int = KeyEvent.VK_W,
                         down: Int = KeyEvent.VK_S,
                         left: Int = KeyEvent.VK_A,
                         right: Int = KeyEvent.VK_D,
                        ) {

  val id:Int  = gameInstance.spawnPlayer()

  val pressed: mutable.Set[Int] = mutable.Set()

  jf.addKeyListener{new KeyListener {
    override def keyTyped(e: KeyEvent): Unit = {

    }
    override def keyPressed(e: KeyEvent): Unit = {
      pressed += e.getKeyCode
      setMoveDirection()
    }
    override def keyReleased(e: KeyEvent): Unit = {
      pressed -= e.getKeyCode
      setMoveDirection()
    }
  }}

  def setMoveDirection():Unit = {
    val u = if(pressed.contains(up)) V2(0, -1) else V2(0, 0)
    val d = if(pressed.contains(down)) V2(0, 1) else V2(0, 0)
    val l = if(pressed.contains(left)) V2(-1, 0) else V2(0, 0)
    val r = if(pressed.contains(right)) V2(1, 0) else V2(0, 0)
    gameInstance.setMoveDirection(u + d + l + r, id)
  }

}
