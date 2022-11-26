package NNAgar.neural

import NNAgar.game.GameModel.{Game, Player}
import NNAgar.game.{GameInstance, Helpers, V2}
import NNAgar.neural.GenomeOps.Genome

object NeuralPlayer{
  
}

class NeuralPlayer(val gameInstance: GameInstance,
                   val genome: Genome,
                   val neuralNet: NeuralNet,
                   val vision: (Game, Int) => IndexedSeq[Double],
                   val control: (GameInstance, Int, IndexedSeq[Double]) => Unit
                  ) {

  val pId: Int = gameInstance.spawnPlayer()
  def tick(): Unit = {
    if (player.alive) {
      val input = vision(gameInstance.g, pId)
      val nnOut = neuralNet.calculate(input)
      control(gameInstance, pId, nnOut)
    }
  }
  
  def player: Player = gameInstance.player(pId)
}
