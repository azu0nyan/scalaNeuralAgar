import java.awt.Graphics2D
import java.awt.image.BufferedImage
import javax.swing.{JFrame, WindowConstants}

object Main {

  val w = 1920
  val h = 1080

  def main(args: Array[String]): Unit = {
    val jf = new JFrame()
    jf.setSize(w, h); //размер экрана
    jf.setUndecorated(false); //показать заголовок окна
    jf.setTitle("Моя супер программа");
    jf.setVisible(true);
    jf.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
    jf.createBufferStrategy(2);

    import java.awt.Graphics2D
    import java.awt.image.BufferStrategy
    while ( true) {
      val frameLength = 1000 / 60 //пытаемся работать из рассчета  60 кадров в секунду
      val start = System.currentTimeMillis

      val bs = jf.getBufferStrategy
      val g = bs.getDrawGraphics.asInstanceOf[Graphics2D]
      g.clearRect(0, 0, jf.getWidth, jf.getHeight)
      draw(g)
      bs.show()
      g.dispose()


      val end = System.currentTimeMillis
      val len = end - start
      if (len < frameLength) Thread.sleep(frameLength - len)
    }
  }

  def draw(d: Graphics2D): Unit = {
    val bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
//    Render.renderImage(bi)
    d.drawImage(bi, 0, 0, null)

    d.drawLine(900, 300, 500, 200)
  }


}