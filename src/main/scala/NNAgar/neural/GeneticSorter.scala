package NNAgar.neural

import NNAgar.game.{GameInstance, GameToNeuralOps}
import NNAgar.game.GameModel.Game
import NNAgar.neural.GenomeOps.Genome

import scala.collection.mutable


case class GeneticSorterParams(
                                targetPlayers: Int = 20,
                                neuralNetStructure: NeuralNetStructure = NeuralNetStructureImpl(IndexedSeq(2, 2), logisticCurve),
                                bitPerGene: Int = 8,
                                conv: Int => Double = x => x / 255.0,
                                playerVision: (Game, Int) => IndexedSeq[Double] = GameToNeuralOps.playerVision,
                                playerControl: (GameInstance,Int, IndexedSeq[Double] ) => Unit = GameToNeuralOps.playerControl,
                                fitnessFunction: (Game, Int) => Double = GameToNeuralOps.fitnessFunction
                              ) {
  def genomeSizeBytes: Int = neuralNetStructure.workingNeurons + neuralNetStructure.synapsesCount

}

class GeneticSorter(params: GeneticSorterParams = GeneticSorterParams()) {

  val gameInstance: GameInstance = new GameInstance()

  val players: mutable.Buffer[NeuralPlayer] = mutable.Buffer()

  def spawn(genome: Genome): Unit = {
    val nn = GenomeOps.neuralNetFromGenome(genome, params.neuralNetStructure, params.bitPerGene, params.conv)
    players += new NeuralPlayer(gameInstance, genome, nn, params.playerVision, params.playerControl)
  }

  def init(): Unit = {
    for (i <- 0 until params.targetPlayers) {
      val genome = GenomeOps.randomGenome(params.genomeSizeBytes)
      spawn(genome)
    }
  }

  def tick(): Unit = {
    for(p <- players) p.tick()
    gameInstance.tick()

    for(i <- 0 until (params.targetPlayers - players.count(_.player.alive))){
      val ab = players.sortBy(p => params.fitnessFunction(gameInstance.g, p.pId))
      val g = GenomeOps.mix(ab(0).genome, ab(1).genome)
      spawn(g)
    }
  }

}
