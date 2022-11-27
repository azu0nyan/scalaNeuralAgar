package NNAgar.game

import NNAgar.neural.NeuralNetCalculator

import java.awt.{BasicStroke, Color, Graphics2D}

object NeuralNetDrawer {
  def draw(nn: NeuralNetCalculator, x: Int, y: Int, w: Int, h: Int, g: Graphics2D): Unit = {

    g.setColor(Color.BLACK)
    g.fillRect(x, y, w, h)

    def layerX(layerId: Int): Int = x + 20 + (layerId) * (w - 40)  / (if(nn.layerCount > 1) (nn.layerCount - 1) else 1)
    def neuronY(layerId: Int, neuronInLayerId: Int): Int = y + (neuronInLayerId + 1) * h / (nn.layerSizes(layerId) + 1)


    for (l <- 0 until nn.layerCount; n <- 0 until nn.layerSizes(l)) {
      val x = layerX(l)
      val y = neuronY(l, n)
      val nVal = if (nn.lastCalculation.nonEmpty) nn.lastCalculation(nn.neuronId(l, n)) else 0
      val nBiasOrInput = if (l == 0) nVal * 2d - 1 else nn.neurons(nn.neuronId(l, n))
      if (nBiasOrInput > 0) {
        g.setColor(new Color(0, math.min(255 * nBiasOrInput * nBiasOrInput, 255).toInt, 10))
      } else {
        g.setColor(new Color(math.min(255 * nBiasOrInput * nBiasOrInput, 255).toInt, 0, 10))
      }

      g.fillOval(x - 5, y - 5, 10, 10)
    }

    for (Seq(prevLayer, curLayer) <- (0 until nn.layerCount).sliding(2);
         n <- 0 until nn.layerSizes(curLayer);
         s <- 0 until nn.layerSizes(prevLayer)) {
      val sVal = nn.synapses(nn.synapseId(prevLayer, s, n))
      val nVal = if (nn.lastCalculation.nonEmpty) nn.lastCalculation(nn.neuronId(prevLayer, n)) else 0
      if (nVal * sVal > 0) {
        g.setColor(new Color(0, math.min(255 * math.abs(nVal * sVal), 255).toInt, 10))
      } else {
        g.setColor(new Color(math.min(255 * math.abs((nVal * sVal)), 255).toInt, 0, 10))
      }

      val thick = math.abs(nVal * sVal).toFloat
      g.setStroke(new BasicStroke(thick))
      g.drawLine(layerX(prevLayer), neuronY(prevLayer, s), layerX(curLayer), neuronY(curLayer, n))

    }

  }
}
