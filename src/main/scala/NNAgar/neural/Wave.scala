package NNAgar.neural

import NNAgar.game.{GameInstance, GameToNeuralOps}
import NNAgar.neural.GenomeOps.Genome

class Wave(val name:String,val  params: GeneticSorterParams,val  genomes: Seq[Genome]) {
  val gameInstance: GameInstance = new GameInstance(params.gameParams)

  var ticks = 0
  var players: Seq[NeuralPlayer] = Seq()

  for(genom <- genomes.take(params.targetPlayers)) spawn(genom)

  def spawn(genome: Genome): Int = {
    val nn = GenomeOps.neuralNetFromGenome(genome, params.neuralNetStructure, params.bitPerGene, params.conv)
    val np = new NeuralPlayer(gameInstance, genome, nn, params.playerVision, params.playerControl)
    players = np +: players
    np.pId
  }

  def tick(): Unit = {
    if (ticks % 1000 == 0) println(s"Tick $ticks")
    ticks += 1

    for (p <- players) p.tick()
    gameInstance.tick()
    if(params.spawnToReachTarget && params.targetPlayers > gameInstance.gameData.alivePlayers.size) {
      val genome = genomes(gameInstance.gameData.playerCount % genomes.size)
      spawn(genome)
    }


  }

  def topGenomes: Seq[Genome] = topPlayers.map(_.genome).distinct  
  
  def topPlayers: Seq[NeuralPlayer] = players.sortBy(p => - GameToNeuralOps.fitnessFunction(gameInstance.gameData, p.pId))
  
  def maxFitness: Double = GameToNeuralOps.fitnessFunction(gameInstance.gameData, topPlayers.head.pId)
  
  def avgFitness: Double = players.map(p => GameToNeuralOps.fitnessFunction(gameInstance.gameData, p.pId)).sum / players.size 
  
  def medianFitness: Double = players.map(p => GameToNeuralOps.fitnessFunction(gameInstance.gameData, p.pId))(players.size / 2)  
  
}
