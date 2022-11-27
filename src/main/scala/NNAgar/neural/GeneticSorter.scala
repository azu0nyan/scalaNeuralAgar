package NNAgar.neural

import NNAgar.game.{GameInstance, GameToNeuralOps, V2}
import NNAgar.game.GameModel.{Game, GameParams}
import NNAgar.neural.GenomeOps.Genome

import java.awt.{Color, Font, Graphics2D}
import java.util.concurrent.{CountDownLatch, CyclicBarrier, Semaphore}
import scala.collection.mutable
import scala.util.Random


case class GeneticSorterParams(
                                playersOnMap: Int = 15,
                                spawnToReachTarget: Boolean = true,
                                threads: Int = 6,
                                generationSize: Int = 25 * 6,
                                generationTicks: Int => Int = {
                                  case x if x < 10 => 5 * 60
                                  case x if x < 100 => (x / 2) * 60
                                  case _ => 60 * 100
                                },

                                passToNextGenerationCount: Int = 60,
                                selectTopToMutatate: Int = 120,

                                neuralNetStructure: NeuralNetStructure = NeuralNetStructureImpl(IndexedSeq(27, 12, 4), logisticCurve),
                                bitPerGene: Int = 8,
                                conv: Int => Double = x => (x - 128) / 128.0,
                                playerVision: (Game, Int) => IndexedSeq[Double] = GameToNeuralOps.playerVision,
                                playerControl: (GameInstance, Int, IndexedSeq[Double]) => Unit = GameToNeuralOps.playerControl,
                                fitnessFunction: (Game, Int) => Double = GameToNeuralOps.fitnessFunction,

                                gameParams: GameParams = GameParams(area = V2(256, 256), initialFood = 10, foodPerTick = 0.05, initialSize = 10d, sizePerFood = 10d)
                              ) {
  def genomeSizeBytes: Int = neuralNetStructure.workingNeurons + neuralNetStructure.synapsesCount

}


class ConcurrentGeneticSorter(params: GeneticSorterParams = GeneticSorterParams()) {

  //  var waves: Seq[Wave] = Seq(new Wave("Initial", params = params, genomes = for (i <- 0 until params.generationSize) yield GenomeOps.randomGenome(params.genomeSizeBytes)))
  //
  var sleep: Long = 0
  var drawGame: Boolean = true

  //  def wave: Wave = waves.last

  var history: Seq[Seq[Wave]] = Seq()
  var currentWaves: Seq[Wave] = Seq()

  var c: CountDownLatch = new CountDownLatch(0)

  def init(): Unit = {

  }

  def startWave(genomes: Seq[Genome]): Unit = {
    println(s"Starting wave ${history.size}")

    c = new CountDownLatch(params.threads)
    val perThread = genomes.size / params.threads
    val genomesPerThread = genomes.sliding(perThread, perThread).toSeq
    println(genomesPerThread.size)
    currentWaves = for (i <- 0 until params.threads) yield {
      val wave = new Wave(s"w:${history.size} t:$i", params, genomesPerThread(i))
      new Thread(() => {
        for (i <- 0 until params.generationTicks(history.size)) {
          wave.tick()
          if(sleep != 0) Thread.sleep(sleep)
        }
        c.countDown()
      }).start()
      wave
    }
    history = history :+ currentWaves


  }

  def tick(): Unit = {
    if(c.getCount == 0) {
      if(history.isEmpty) {
        val genomes = for(i <- 0 until params.generationSize) yield GenomeOps.randomGenome(params.genomeSizeBytes)
        startWave(genomes)
      } else {
        val topGenomes = currentWaves.flatMap(_.topGenomes.take(params.selectTopToMutatate / params.threads))
        val topToNextWave = currentWaves.flatMap(_.topGenomes.take(params.passToNextGenerationCount / params.threads))
        val r = new Random()
        val newGenomes = (for(i <- 0 until (params.generationSize - params.passToNextGenerationCount))
          yield GenomeOps.mix(topGenomes(r.nextInt(topGenomes.size)), topGenomes(r.nextInt(topGenomes.size)))).map(GenomeOps.flipRandomBits(_, r.nextInt(64)))

        val shuffled = new Random().shuffle(topToNextWave ++ newGenomes)

        startWave(shuffled)
      }
    }
  }

  def draw(g: Graphics2D): Unit = {
        if (drawGame) {
          val rc = math.sqrt(params.threads).ceil.toInt

          val x = 10
          val y = 40
          val padding = 20
          val size = 1000 / rc - padding

          for((w, i) <- currentWaves.zipWithIndex){
            val xi = i % rc
            val yi = i / rc
            w.gameInstance.draw(g, x + xi * (size + padding), y + yi * (size + padding), size)
           }

        }

        drawWavesStats(g)
  }

    def drawWavesStats(g: Graphics2D): Unit = {
      val x = 1000
      val y = 40
      val dy = 30
      val maxLines = 30

      g.setColor(Color.WHITE)
      g.fillRect(x, y, 300, dy * math.min(history.size, maxLines))

      g.setColor(Color.BLACK)
      g.setFont(new Font("", Font.PLAIN, 12))
      for ((cw, i) <- history.reverse.zipWithIndex.take(maxLines)) {

        val avgFit = cw.map(_.avgFitness).sum / cw.size
        val maxFit = cw.map(_.maxFitness).max
        g.drawString(cw.head.name, x + 10, y + i * dy + 25)
        g.drawString(f"av: ${avgFit}%.1f max ${maxFit}%.1f", x + 100, y + i * dy + 25)

      }
    }

}


class SingleThreadGeneticSorter(params: GeneticSorterParams = GeneticSorterParams()) {

  var waves: Seq[Wave] = Seq(new Wave("Initial", params = params, genomes = for (i <- 0 until params.generationSize) yield GenomeOps.randomGenome(params.genomeSizeBytes)))

  var sleep: Long = 0
  var drawGame: Boolean = true

  def wave: Wave = waves.last


  def init(): Unit = {

  }

  def tick(): Unit = {
    wave.tick()
    if (wave.ticks >= params.generationTicks(waves.size)) {
      println(s"New wave ${waves.size}")
      val topGenomes = wave.topGenomes.take(params.selectTopToMutatate)
      val r = new Random()
      val nextWaveGenomes = topGenomes.take(params.passToNextGenerationCount) ++
        (for (i <- 0 until (params.generationSize - params.passToNextGenerationCount))
          yield GenomeOps.mix(topGenomes(r.nextInt(topGenomes.size)), topGenomes(r.nextInt(topGenomes.size)))).map(GenomeOps.flipRandomBits(_, r.nextInt(64)))
      waves = waves :+ new Wave(s"wave: ${waves.size}", params, nextWaveGenomes)
    }
    if (sleep > 0) Thread.sleep(sleep)
  }

  def draw(g: Graphics2D): Unit = {
    if (drawGame) {
      wave.gameInstance.draw(g, 20, 20 , 1000)
    }

    drawWavesStats(g)
  }

  def drawWavesStats(g: Graphics2D): Unit = {
    val x = 1000
    val y = 100
    val dy = 30
    val maxLines = 30

    g.setColor(Color.WHITE)
    g.fillRect(x, y, 300, dy * waves.size)

    g.setColor(Color.BLACK)
    g.setFont(new Font("", Font.PLAIN, 12))
    for ((w, i) <- waves.reverse.zipWithIndex.take(maxLines)) {
      g.drawString(w.name, x + 10, y + i * dy + 25)
      g.drawString(f"av: ${w.avgFitness}%.1f med: ${w.medianFitness}%.1f max ${w.maxFitness}%.1f", x + 100, y + i * dy + 25)

    }
  }

}
