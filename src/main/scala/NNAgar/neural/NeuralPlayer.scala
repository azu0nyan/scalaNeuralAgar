package NNAgar.neural

import NNAgar.game.GameModel.Game
import NNAgar.game.{GameInstance, Helpers, V2}

class NeuralPlayer(gameInstance: GameInstance,
                   neuralNet: NeuralNet,
                   neuralInputForPlayer: (Game, Int) => IndexedSeq[Double],
                   outputToAction: (GameInstance, Seq[Double]) => Unit
                  ) {

  val pId: Int = gameInstance.spawnPlayer()
  def tick(): Unit = {
    if (gameInstance.player(pId).alive) {
      val input = neuralInputForPlayer(gameInstance.g, pId)
      val nnOut = neuralNet.calculate(input)
      outputToAction(gameInstance, nnOut)
    }
  }
}
