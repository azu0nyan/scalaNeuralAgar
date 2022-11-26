package NNAgar.neural


def logisticCurve(x: Double): Double = 1d / (1d + math.exp(-x))


trait NeuralNetStructure {
  def layerSizes: IndexedSeq[Int]

  val layerCount: Int = layerSizes.length

  val neuronCount: Int = layerSizes.sum

  val layerNeuronBegin: IndexedSeq[Int] = layerSizes
    .foldLeft(Seq(0)) { case (s, l) => s :+ (s.last + l) }.toIndexedSeq

  val layerSunapseBegin: IndexedSeq[Int] = layerSizes.sliding(2)
    .map { case Seq(a, b) => a * b }
    .foldLeft(Seq(0)) { case (s, l) => s :+ (s.last + l) }.toIndexedSeq


  val synapsesCount: Int = layerSunapseBegin.last

  val workingNeurons: Int = layerSizes.sum - layerSizes.head

  @inline def neuronId(layerId: Int, neuronInLayerId: Int): Int = layerNeuronBegin(layerId) + neuronInLayerId

  @inline def synapseId(prevLayerId: Int, prevNeuronInLayerId: Int, neuronInLayerId: Int): Int =
    layerSunapseBegin(prevLayerId) + layerSizes(prevLayerId) * neuronInLayerId + prevNeuronInLayerId
}

case class NeuralNetStructureImpl(layerSizes: IndexedSeq[Int]) extends NeuralNetStructure

trait NeuralNetCalculator extends NeuralNetStructure {
  def neurons: IndexedSeq[Double]
  def synapses: IndexedSeq[Double]
  def activation: Double => Double

  def calculate(in: Seq[Double]): Seq[Double] = {
    val calculated = Array.ofDim[Double](neuronCount)
    for (i <- in.indices) calculated(i) = in(i)

    for (layer <- 1 until layerCount;
         prevLayerNeuronOffset = layerNeuronBegin(layer - 1);
         neuronInLayer <- 0 until layerSizes(layer);
         nId = neuronId(layer, neuronInLayer);
         c0 = neurons(nId);
         s = (for (prevLayerNeuron <- 0 until layerSizes(layer - 1);
                   linkedNeuron = calculated(prevLayerNeuronOffset + prevLayerNeuron);
                   synapse = synapses(synapseId(layer - 1, prevLayerNeuron, neuronInLayer))
                   ) yield linkedNeuron * synapse).sum + c0;
         y = activation(s)
         ) calculated(nId) = y

    calculated.takeRight(layerSizes.last)
  }

}

object NeuralNet {
  def apply(layerSizes: IndexedSeq[Int],
            neuronAndSynapses: IndexedSeq[Double],
            activation: Double => Double): NeuralNet = {
    val nc = NeuralNetStructureImpl(layerSizes).neuronCount

    NeuralNet(layerSizes,
      neuronAndSynapses.take(nc),
      neuronAndSynapses.drop(nc),
      activation)
  }
}

case class NeuralNet(layerSizes: IndexedSeq[Int],
                     neurons: IndexedSeq[Double],
                     synapses: IndexedSeq[Double],
                     activation: Double => Double) extends NeuralNetCalculator {

}


