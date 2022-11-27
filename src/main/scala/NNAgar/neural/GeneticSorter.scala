package NNAgar.neural

import NNAgar.game.{GameInstance, GameToNeuralOps}
import NNAgar.game.GameModel.{Game, GameParams}
import NNAgar.neural.GenomeOps.Genome

import java.awt.{Color, Font, Graphics2D}
import scala.collection.mutable
import scala.util.Random


case class GeneticSorterParams(
                                targetPlayers: Int = 50,
                                spawnToReachTarget: Boolean = false,

                                generationSize: Int = 50,
                                generationTicks: Int => Int = {
                                  case x if x < 10 => 5 * 60
                                  case x if x < 100 => (x / 2) * 60
                                  case _ => 60 * 100
                                },
                                passToNextGenerationCount: Int = 10,
                                selectTopToMutatate: Int = 20,
                                neuralNetStructure: NeuralNetStructure = NeuralNetStructureImpl(IndexedSeq(27, 12,  4), logisticCurve),
                                bitPerGene: Int = 8,
                                conv: Int => Double = x => (x - 128) / 128.0,
                                playerVision: (Game, Int) => IndexedSeq[Double] = GameToNeuralOps.playerVision,
                                playerControl: (GameInstance,Int, IndexedSeq[Double] ) => Unit = GameToNeuralOps.playerControl,
                                fitnessFunction: (Game, Int) => Double = GameToNeuralOps.fitnessFunction,

                                gameParams: GameParams = GameParams()
                              ) {
  def genomeSizeBytes: Int = neuralNetStructure.workingNeurons + neuralNetStructure.synapsesCount

}

class GeneticSorter(params: GeneticSorterParams = GeneticSorterParams()) {

  var waves: Seq[Wave] = Seq(new Wave("Initial", params = params, genomes = for(i <- 0 until params.generationSize) yield GenomeOps.randomGenome(params.genomeSizeBytes)))

  var sleep: Long = 0

  def wave: Wave = waves.last


  def init(): Unit = {

  }

  def tick(): Unit = {
    wave.tick()
    if(wave.ticks >= params.generationTicks(waves.size)){
      println(s"New wave ${waves.size}")
      val topGenomes = wave.topGenomes.take(params.selectTopToMutatate)
      val r = new Random()
      val nextWaveGenomes = topGenomes.take(params.passToNextGenerationCount) ++
        (for(i <- 0 until (params.generationSize - params.passToNextGenerationCount))
          yield GenomeOps.mix(topGenomes(r.nextInt(topGenomes.size)), topGenomes(r.nextInt(topGenomes.size)))).map(GenomeOps.flipRandomBits(_, r.nextInt(64)))
      waves = waves :+ new Wave(s"wawe: ${waves.size}", params, nextWaveGenomes)
    }
    if(sleep > 0) Thread.sleep(sleep)
  }

  def draw(g: Graphics2D): Unit = {
    wave.gameInstance.draw(g)

    val x = 1000
    val y = 100
    val dy = 50

    g.setColor(Color.WHITE)
    g.fillRect(x, y, 300, dy * waves.size)

    g.setColor(Color.BLACK)
    g.setFont(new Font("", Font.PLAIN, 12))
    for((w, i) <- waves.reverse.zipWithIndex) {
      g.drawString(w.name, x + 10, y + i * dy + 25)
      g.drawString(f"av: ${w.avgFitness}%.1f med: ${w.medianFitness}%.1f max ${w.maxFitness}%.1f", x + 100, y + i * dy + 25)

    }
  }

}
