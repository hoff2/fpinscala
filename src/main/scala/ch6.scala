import scala.util.Random

object Ch6 {

  trait RNG { def nextInt: (Int, RNG) }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, g) = rng.nextInt
    (if (i == Int.MinValue) 0 else scala.math.abs(i), g)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, g) = rng.nextInt
    (i.toDouble / Int.MaxValue, g)
  }

  def main(args: Array[String]):Unit = {
    println("========================================")
    val rng = SimpleRNG(239847923)
    println(nonNegativeInt(rng))
    println(double(rng))
  }
}
