package NNAgar.neural


def logisticCurve(x: Double): Double = 1d / (1d + math.exp(-x))

case class NeuralNet(layerSizes: Seq[Int] = Seq(1, 1),
                     synapses: Seq[Double], neurons: Seq[Double],
                     offset: Double = 0.1,
                     activation: Double => Double = logisticCurve) {

  val layerCount: Int = layerSizes.length

  val neuronCount: Int = layerSizes.sum

  val layerNeuronBegin: Seq[Int] = layerSizes
    .foldLeft(Seq(0)) { case (s, l) => s :+ (s.last + l) }

  val layerSunapseBegin: Seq[Int] = layerSizes.sliding(2)
    .map { case Seq(a, b) => a * b }
    .foldLeft(Seq(0)) { case (s, l) => s :+ (s.last + l) }


  val synapsesCount: Int = layerSunapseBegin.last

  val workingNeurons: Int = layerSizes.sum - layerSizes.head

  def calculate(in: Seq[Double]): Seq[Double] = {
    val calculated = Array.ofDim[Double](neuronCount)
    for (i <- in.indices) calculated(i) = in(i)

    for (layer <- 1 until layerCount;
         layerSize = layerSizes(layer);
         prevLayerSize = layerSizes(layer - 1);
         prevLayerNeuronOffset = layerNeuronBegin(layer - 1);
         synapsesOffset = layerSunapseBegin(layer - 1);
         neuronInLayer <- 0 until layerSize;
         neuronId = layerNeuronBegin(layer) + neuronInLayer;

         c0 = neurons(neuronId);
         s = (for (i <- 0 until layerSize;
                   linkedNeuron = calculated(prevLayerNeuronOffset + i);
                   synapse = synapses(synapsesOffset + prevLayerSize * neuronInLayer + i)
                   ) yield linkedNeuron * synapse).sum + c0;
         y = activation(s)
         ) calculated(neuronId) = y

    calculated.takeRight(layerSizes.last)
  }


}


