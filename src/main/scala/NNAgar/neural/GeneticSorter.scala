package NNAgar.neural

import NNAgar.game.{GameInstance, GameToNeuralOps, NeuralNetDrawer, V2}
import NNAgar.game.GameModel.{Game, GameParams}
import NNAgar.neural.GenomeOps.Genome

import java.awt.{Color, Font, Graphics2D}
import java.util.concurrent.{CountDownLatch, CyclicBarrier, Semaphore}
import scala.collection.mutable
import scala.util.Random


case class GeneticSorterParams(
                                playersOnMap: Int = 1,
                                spawnToReachTarget: Boolean = false,
                                threads: Int = 64,
                                generationSize: Int = 64,
                                generationTicks: Int => Int = x => 300,

                                passToNextGenerationCount: Int = 4,
                                selectTopToMutatate: Int = 12,
                                flipBits: Double => Int = x => 256,
                                numCuts: Int = 8,

                                wavesToRemember: Int = 25,

                                neuralNetStructure: NeuralNetStructure =
                                NeuralNetStructureImpl(IndexedSeq(
                                  GameToNeuralOps.visionSize, 8, 8, 4), logisticCurve),
                                bitPerGene: Int = 8,
                                conv: Int => Double = x => (x - 128) / 128.0,
                                playerVision: (Game, Int) => IndexedSeq[Double] = GameToNeuralOps.playerVision,
                                playerControl: (GameInstance, Int, IndexedSeq[Double]) => Unit = GameToNeuralOps.playerControl,
                                fitnessFunction: (Game, Int) => Double = GameToNeuralOps.fitnessFunction,

                                gameParams: GameParams = GameParams(area = V2(256, 256),
                                  initialFood = 10,
                                  maxFood = 10,
                                  foodPerTick = 0.1,
                                  initialSize = 25d,
                                  sizePerFood = 10d,
                                  dSizePerTick = 0.1,

                                  //                                  initialObstacles = 12,
                                  //                                  obstacleMin = V2(32, 32),
                                  //                                  obstacleMax = V2(32, 32),

                                  initialObstacles = 0,
                                  obstacleMin = V2(256 / 5, 256 / 5),
                                  obstacleMax = V2(256 / 5, 256 / 5),

                                  obstacleGridSize = 5,

                                  speed = x => math.max(10, 400 - (x / 4))
                                )
                              ) {
  def genomeSizeBytes: Int = neuralNetStructure.workingNeurons + neuralNetStructure.synapsesCount

}


class ConcurrentGeneticSorter(val params: GeneticSorterParams = GeneticSorterParams()) {

  //  var waves: Seq[Wave] = Seq(new Wave("Initial", params = params, genomes = for (i <- 0 until params.generationSize) yield GenomeOps.randomGenome(params.genomeSizeBytes)))
  //
  var drawGame: Boolean = true

  var sleep: Long = 20
  var pause: Boolean = false
  var pauseOnNewWave: Boolean = false
  var overrideRoundTicks: Option[Int] = None

  var showFieldId: Int = -1

  //  def wave: Wave = waves.last

  var waveId = 0
  var history: Seq[Seq[Wave]] = Seq()
  var currentWaves: Seq[Wave] = Seq()

  var c: CountDownLatch = new CountDownLatch(0)


  def init(): Unit = {

  }

  def startWave(genomes: Seq[Genome]): Unit = {
    println(s"Starting wave ${history.size}")
    if (pauseOnNewWave) {
      pause = true
    }
    c = new CountDownLatch(params.threads)
    val perThread = genomes.size / params.threads
    val genomesPerThread = genomes.sliding(perThread, perThread).toSeq
    println(genomesPerThread.size)
    currentWaves = for (i <- 0 until params.threads) yield {
      val wave = new Wave(s"w:${waveId} t:$i", params, genomesPerThread(i))
      new Thread(() => {
        var i = 0
        while (i < {
          overrideRoundTicks match
            case Some(o) => o
            case None => params.generationTicks(waveId)
        }) {
          if (!pause) {
            wave.tick()
            if (wave.gameInstance.gameData.alivePlayers.isEmpty) {
              i = overrideRoundTicks match
                case Some(o) => o
                case None => params.generationTicks(waveId)
            }
            i = i + 1
          } else {
            Thread.sleep(1)
          }
          if (sleep != 0) Thread.sleep(sleep)
        }
        c.countDown()
      }).start()
      wave
    }
    history = history :+ currentWaves
    waveId = waveId + 1
    if (history.size > params.wavesToRemember) history = history.drop(history.size - params.wavesToRemember)
    selectedPlayerId = None
  }

  def tick(): Unit = {
    if (c.getCount == 0) {
      if (history.isEmpty) {
        val genomes = for (i <- 0 until params.generationSize) yield GenomeOps.randomGenome(params.genomeSizeBytes)
        startWave(genomes)
      } else {
        val genomes = currentWaves.flatMap(_.playersZipFitness).map { case (p, f) => (p.genome, f) }.sortBy(-_._2)
        val topGenomesToMutate = genomes.take(params.selectTopToMutatate).map(_._1)

        val topToNextWave = genomes.take(params.passToNextGenerationCount).map(_._1)

        val r = new Random()
        val avgFit = genomes.map(_._2).sum / genomes.size
        val maxFit = genomes.head._2
        println(s"Wave $waveId finished avgFit: $avgFit maxFit: $maxFit")

        val newGenomes = (for (i <- 0 until (params.generationSize - params.passToNextGenerationCount))
          yield GenomeOps.mix(topGenomesToMutate(r.nextInt(topGenomesToMutate.size)), topGenomesToMutate(r.nextInt(topGenomesToMutate.size)), params.numCuts))
          .map(GenomeOps.flipRandomBits(_, r.nextInt(params.flipBits(avgFit))))

        val shuffled = new Random().shuffle(topToNextWave ++ newGenomes)

        startWave(shuffled)
      }
    }
  }

  val gameFieldRowColums = math.sqrt(params.threads).ceil.toInt
  val gameFieldX = 10
  val gameFieldY = 40
  val gameFieldPadding = 20
  val gameFieldTotalWH = 1000

  val gameFieldSize = gameFieldTotalWH
  val gameFieldScale = gameFieldSize / params.gameParams.area.x

  val gameSmallFieldSize = gameFieldTotalWH / gameFieldRowColums - gameFieldPadding
  val gameSmallFieldSizeWithPadding = gameSmallFieldSize + gameFieldPadding
  val gameSmallFieldScale = gameSmallFieldSize / params.gameParams.area.x

  var selectedPlayerId: Option[Int] = None
  var selectedPlayerWave: Int = 0

  def draw(g: Graphics2D): Unit = {
    if (selectedPlayerId.isEmpty) {

      val wave = if (showFieldId == -1) currentWaves.maxBy(w => w.players.maxBy(p => p.player.size).player.size)
      else currentWaves(showFieldId % currentWaves.size)

      val pl = wave.players.maxBy(_.player.size)
      selectedPlayerWave = currentWaves.indexOf(wave)
      selectedPlayerId = Some(pl.pId)

    }

    if (drawGame) {
      if (showFieldId == -1) {
        for ((w, i) <- currentWaves.zipWithIndex) {
          val xi = i % gameFieldRowColums
          val yi = i / gameFieldRowColums

          val fx = gameFieldX + xi * gameSmallFieldSizeWithPadding
          val fy = gameFieldY + yi * gameSmallFieldSizeWithPadding
          w.gameInstance.draw(g, fx, fy, gameSmallFieldScale,
            if (i == selectedPlayerWave) selectedPlayerId.toSeq else Seq())
          if (selectedPlayerId.nonEmpty && i == selectedPlayerWave) {
            GameToNeuralOps.playerVisionWithDrawing(w.gameInstance.gameData, selectedPlayerId.get, Some(g),
              fx, fy, gameSmallFieldScale
            )
          }
        }
      } else {
        currentWaves(showFieldId % currentWaves.size).gameInstance.draw(g, gameFieldX, gameFieldY, gameFieldScale,
          if (showFieldId == selectedPlayerWave) selectedPlayerId.toSeq else Seq())
        if (selectedPlayerId.nonEmpty && showFieldId == selectedPlayerWave) {
          GameToNeuralOps.playerVisionWithDrawing(currentWaves(showFieldId % currentWaves.size).gameInstance.gameData, selectedPlayerId.get, Some(g),
            gameFieldX, gameFieldY, gameFieldScale
          )

        }
      }


      drawWavesStats(g)

      drawSelectedNN(g)
    }
  }

  def drawSelectedNN(g: Graphics2D): Unit = {
    if (selectedPlayerId.nonEmpty) {
      currentWaves(selectedPlayerWave).players.find(_.pId == selectedPlayerId.get) match
        case Some(p) =>
          NeuralNetDrawer.draw(p.neuralNet, 1000, 500, 600, 500, g)
        case None =>
    }
  }

  def click(x: Int, y: Int): Unit = {
    if (showFieldId == -1) {
      val xi = (x - gameFieldX) / gameSmallFieldSizeWithPadding
      val yi = (y - gameFieldY) / gameSmallFieldSizeWithPadding

      val xC = (x - gameFieldX) % gameSmallFieldSizeWithPadding
      val yC = (y - gameFieldY) % gameSmallFieldSizeWithPadding
      val pos = V2(xC / gameSmallFieldScale, yC / gameSmallFieldScale)

      val id = xi + yi * gameFieldRowColums
      if (0 <= id && id < currentWaves.size) {
        val w = currentWaves(id)
        selectedPlayerId = w.players.filter(_.player.alive).minByOption(p => (p.player.pos - pos).length).map(_.pId)
        selectedPlayerWave = id
      }
    } else {
      val xC = (x - gameFieldX) % gameFieldSize
      val yC = (y - gameFieldY) % gameFieldSize
      val pos = V2(xC / gameFieldScale, yC / gameFieldScale)
      selectedPlayerId = currentWaves(showFieldId % currentWaves.size).players.filter(_.player.alive).minByOption(p => (p.player.pos - pos).length).map(_.pId)
      selectedPlayerWave = showFieldId % currentWaves.size
    }
  }

  def drawWavesStats(g: Graphics2D): Unit = {
    val x = 1000
    val y = 40
    val dy = 30
    val maxLines = 30

    g.setColor(Color.WHITE)
    g.fillRect(x, y, 400, dy * math.min(history.size, maxLines))

    g.setColor(Color.BLACK)
    g.setFont(new Font("", Font.PLAIN, 12))
    for ((cw, i) <- history.reverse.zipWithIndex.take(maxLines)) {

      val avgFit = cw.map(_.avgFitness).sum / cw.size
      val maxFit = cw.map(_.maxFitness).max

      g.drawString(cw.head.name, x + 10, y + i * dy + 25)
      g.drawString(f"av: ${avgFit}%.1f max ${maxFit}%.1f", x + 100, y + i * dy + 25)

    }

    g.drawString(currentWaves.map(_.ticks).mkString(" "), x + 300, y + dy)

  }
}

/*
  class SingleThreadGeneticSorter(params: GeneticSorterParams = GeneticSorterParams()) {

    var waves: Seq[Wave] = Seq(new Wave("Initial", params = params, genomes = for (i <- 0 until params.generationSize)
      yield GenomeOps.randomGenome(params.genomeSizeBytes)))

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
            yield GenomeOps.mix(topGenomes(r.nextInt(topGenomes.size)), topGenomes(r.nextInt(topGenomes.size)), params.numCuts)).map(GenomeOps.flipRandomBits(_, r.nextInt(64)))
        waves = waves :+ new Wave(s"wave: ${waves.size}", params, nextWaveGenomes)
      }
      if (sleep > 0) Thread.sleep(sleep)
    }

    def draw(g: Graphics2D): Unit = {
      if (drawGame) {
        //      wave.gameInstance.draw(g, 20, 20, 1000, Seq())
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
}*/