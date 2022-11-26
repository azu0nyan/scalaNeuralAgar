package NNAgar.neural

import NNAgar.game.{GameInstance, GameToNeuralOps}
import NNAgar.game.GameModel.Game
import NNAgar.neural.GenomeOps.Genome

import scala.collection.mutable


case class GeneticSorterParams(
                                targetPlayers: Int = 20,
                                neuralNetStructure: NeuralNetStructure = NeuralNetStructureImpl(IndexedSeq(27, 12, 12, 4), logisticCurve),
                                bitPerGene: Int = 8,
                                conv: Int => Double = x => (x - 128) / 128.0 ,
                                playerVision: (Game, Int) => IndexedSeq[Double] = GameToNeuralOps.playerVision,
                                playerControl: (GameInstance,Int, IndexedSeq[Double] ) => Unit = GameToNeuralOps.playerControl,
                                fitnessFunction: (Game, Int) => Double = GameToNeuralOps.fitnessFunction
                              ) {
  def genomeSizeBytes: Int = neuralNetStructure.workingNeurons + neuralNetStructure.synapsesCount

}

class GeneticSorter(params: GeneticSorterParams = GeneticSorterParams()) {

  val gameInstance: GameInstance = new GameInstance()

  val players: mutable.Buffer[NeuralPlayer] = mutable.Buffer()

  def spawn(genome: Genome): Int = {
    val nn = GenomeOps.neuralNetFromGenome(genome, params.neuralNetStructure, params.bitPerGene, params.conv)
    val np = new NeuralPlayer(gameInstance, genome, nn, params.playerVision, params.playerControl)
    players += np
    np.pId
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

      val parent1 = ab(0)
      val parent2 = ab(1)
      val g = GenomeOps.mix(parent1.genome, parent2.genome)
      val gg = GenomeOps.flipRandomBits(g, 10)


      val newId = spawn(gg)
      println(s"Mixing ${parent1.pId}:${params.fitnessFunction(gameInstance.g, parent1.pId)} and ${parent2.pId}:${params.fitnessFunction(gameInstance.g, parent2.pId)} spawned $newId")
    }
  }

}
