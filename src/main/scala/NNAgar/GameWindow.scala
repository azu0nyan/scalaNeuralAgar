package NNAgar



import java.awt.Graphics2D
import javax.swing.{JFrame, WindowConstants}

class GameWindow(preDraw: => Unit = {}, draw: Graphics2D => Unit, var fps: Int  = 60, w:Int = 1920, h: Int = 1080) {
    val jf = new JFrame()
    jf.setSize(w, h); //размер экрана
    jf.setUndecorated(false); //показать заголовок окна
    jf.setTitle("Neural Agar");
    jf.setVisible(true);
    jf.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
    jf.createBufferStrategy(2);

    def loop():Unit = {
      new Thread(() => {
        Thread.sleep(500)
        jf.setSize(w - 1, h )

      }).start()

      import java.awt.Graphics2D
      import java.awt.image.BufferStrategy
      while (true) {

        val frameLength = 1000 / fps //пытаемся работать из рассчета  60 кадров в секунду
        val start = System.currentTimeMillis

        val bs = jf.getBufferStrategy
        val g = bs.getDrawGraphics.asInstanceOf[Graphics2D]
        g.clearRect(0, 0, jf.getWidth, jf.getHeight)

        preDraw
        draw(g)

        bs.show()
        g.dispose()


        val end = System.currentTimeMillis
        val len = end - start
       // if (len < frameLength) Thread.sleep(frameLength - len)
      }
    }

    def startLoopSeparateThread(): Unit = {
      new Thread(() => loop()).start()
    }


}
