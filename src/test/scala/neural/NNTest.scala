package neural

import NNAgar.neural.NeuralNet
import org.scalatest.funsuite.AnyFunSuite

class NNTest extends AnyFunSuite {
  test("1 1 network") {
    val n = NeuralNet(
      layerSizes = Array(1, 1),
      neurons = Array(0, 2),
      synapses = Array(3),
      activation = x => x
    )

    assert(n.calculate(Seq(5)).size == 1)
    assert(n.calculate(Seq(5)).head == 5 * 3 + 2)
  }

  test("2 1 network") {
    val n = NeuralNet(
      layerSizes = Array(2, 1),
      neurons = Array(0, 0, 2),
      synapses = Array(3, 8),
      activation = x => x
    )

    assert(n.calculate(Seq(5, 6)).size == 1)
    assert(n.calculate(Seq(5, 6)).head == 5 * 3 + 6 * 8 + 2)
  }


  test("2 2 network") {
    val n = NeuralNet(
      layerSizes = Array(2, 2),
      neurons = Array(0, 0, 2, 3),
      synapses = Array(3, 8, 7, 9),
      activation = x => x
    )

    assert(n.calculate(Seq(5, 6)).size == 2)
    assert(n.calculate(Seq(5, 6)) == Seq(5 * 3 + 6 * 8 + 2, 5 * 7 + 6 * 9 + 3))
  }

  test("2 2 1 network") {
    val n = NeuralNet(
      layerSizes = Array(2, 2, 1),
      neurons = Array(0, 0, 2, 3, 5),
      synapses = Array(3, 8, 7, 9, 11, 13),
      activation = x => x
    )

    assert(n.calculate(Seq(5, 6)).size == 1)
    assert(n.calculate(Seq(5, 6)).head == 5 + (5 * 3 + 6 * 8 + 2) * 11 + (5 * 7 + 6 * 9 + 3) * 13)
  }

}
