package NNAgar.neural

import NNAgar.game.{GameInstance, Helpers, V2}

class NeuralPlayer(gameInstance: GameInstance) {

  val pId: Int = gameInstance.spawnPlayer()
  def tick(): Unit = {
    gameInstance.setMoveDirection(Helpers.randomInArea(V2(2,2)) - V2(1, 1), pId)    
  }
}
