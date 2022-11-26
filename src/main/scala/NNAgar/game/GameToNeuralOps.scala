package NNAgar.game

import NNAgar.game.GameModel.{Game, Player}

object GameToNeuralOps {
  def playerVision(g: Game, pId: Int): IndexedSeq[Double] = IndexedSeq(1)

  def fitnessFunction(g: Game, pId: Int): Double = {
    val player = g.player(pId)
    player.size + player.distanceTraveled * 20 + (player.deadAt.getOrElse(g.tick) - player.spawnedAt)
  }

  def playerControl(gi: GameInstance, pId: Int, act: IndexedSeq[Double]): Unit = {
    val dir = V2(act(0), act(1)) - V2(0.5, 0.5)
    gi.setMoveDirection(dir, pId)
  }
}
