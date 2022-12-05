package NNAgar.neural

import scala.collection.mutable
import scala.util.Random

object GenomeOps {
  type Genome = IndexedSeq[Byte]

  def randomGenome(length: Int): Genome = {
    new Random().nextBytes(length)
  }

  def toGenes[GENE](genome: Genome, toGene: Int => GENE, bitsPerGene: Int = 8): IndexedSeq[GENE] = {
    var cur = 0
    var cId = 0
    val res: mutable.Buffer[GENE] = mutable.Buffer()

    for (byte <- genome; bit <- 0 until 8) {
      val toSet = (byte >> bit) & 1
      val geneBit = cId % bitsPerGene
      cur = cur | (toSet << geneBit)
      cId += 1
      if (cId % bitsPerGene == 0) {
        res += toGene(cur)
        cur = 0
      }
    }
    res.toIndexedSeq
  }


  def mix(a: Genome, b: Genome, cuts: Int): Genome = {
    val r = new Random()
    val cutPosR = Seq.fill(cuts)(r.nextInt(a.size * 8)).distinct.sorted
    val cutPos = 0 +: cutPosR :+ (a.size * 8)

    val aStr = toStringGenomeBinary(a, None)
    val bStr = toStringGenomeBinary(b, None)

    val newGenome = cutPos.sliding(2).zipWithIndex.map {
      case (Seq(f, t), id) if id % 2 == 0 => aStr.substring(f, t)
      case (Seq(f, t), _) => bStr.substring(f, t)
    }.mkString("")
//    println(newGenome)

    newGenome.sliding(8, 8).map { s =>
      var b: Byte = 0
      for (i <- 0 until 8 if s(i) == '1') {
        b = (b | (1 << i)).toByte
      }
      b
    }.toIndexedSeq
  }


  def mixBitwise(a: Genome, b: Genome): Genome = {
    val r = new Random()
    val res = r.nextBytes(a.size)
    for (i <- res.indices)
      res(i) = (a(i) & res(i) | b(i) & ~res(i)).toByte

    res
  }


  def flipRandomBits(a: Genome, bits: Int): Genome = {
    val res = a.toArray
    val r = new Random()
    for (_ <- 0 until bits) {
      val rbyte = r.nextInt(a.size)
      val rBit = r.nextInt(8)
      val bit = (res(rbyte) >> rBit) & 1
      res(rbyte) = (if (bit == 1) res(rbyte) & ~(1 << rBit)
      else res(rbyte) | (1 << rBit)).toByte
    }
    res
  }

  def toStringGenomeBinary(a: Genome, groupSizeOpt: Option[Int] = Some(8)): String = {
    val res = new StringBuilder()
    for (i <- a.indices; b <- 0 until 8) {
      res.append((a(i) >> b) & 1)
      for (groupSize <- groupSizeOpt) {
        if ((i * 8 + b) % groupSize == groupSize - 1) res.append(" ")
      }
    }
    res.toString()
  }

  def neuralNetFromGenome(g: Genome, str: NeuralNetStructure, bitsPerGene: Int = 8, conv: Int => Double): NeuralNet = {
    NeuralNet(str.layerSizes, toGenes(g, conv, bitsPerGene), str.activation)
  }
}
