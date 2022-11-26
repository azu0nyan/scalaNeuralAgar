package neural

import NNAgar.neural.GenomeOps
import NNAgar.neural.GenomeOps.Genome
import org.scalatest.funsuite.AnyFunSuite

class GenomeTest extends AnyFunSuite{
  test("Mix 1"){
    println(s"mix 1")
    val a: Genome = IndexedSeq(255.toByte,255.toByte)
    val b: Genome = IndexedSeq(0.toByte,0.toByte)
    val m = GenomeOps.mix(a, b)
    println(GenomeOps.toStringGenomeBinary(a))
    println(GenomeOps.toStringGenomeBinary(b))
    println(GenomeOps.toStringGenomeBinary(m))

  }

  test("Mix 2") {
    println(s"Mix 2")
    val a: Genome = IndexedSeq(255.toByte, 255.toByte)
    val b: Genome = IndexedSeq(128.toByte, 31.toByte)
    val m = GenomeOps.mix(a, b)
    println(GenomeOps.toStringGenomeBinary(a))
    println(GenomeOps.toStringGenomeBinary(b))
    println(GenomeOps.toStringGenomeBinary(m))

  }

  test("Flip n") {
    println(s"FLIP")
    val a: Genome = IndexedSeq(255.toByte, 255.toByte, 0.toByte, 0.toByte)
    val m = GenomeOps.flipRandomBits(a, 16)

    println(GenomeOps.toStringGenomeBinary(a))
    println(GenomeOps.toStringGenomeBinary(m))

  }

  test("To Genes") {
    val a: Genome = IndexedSeq(255.toByte, 128.toByte, 31.toByte, 0.toByte, 134.toByte)
    println(GenomeOps.toGenes(a, x => x, 8))
    println(GenomeOps.toGenes(a, x => x, 16))

  }

  test("To Genes 2" ) {
    val a: Genome = IndexedSeq(255.toByte, 255.toByte, 128.toByte, 31.toByte, 0.toByte, 134.toByte)
    println(GenomeOps.toGenes(a, x => x / 255d, 8))
    println(GenomeOps.toGenes(a, x => x, 10))
    println(GenomeOps.toGenes(a, x => x / 1024d, 10))
    println(GenomeOps.toGenes(a, x => x / 16536d, 16))

  }
}
