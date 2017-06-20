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

  // 6.1 - 6.4
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, g) = rng.nextInt
    ( if (i == Int.MinValue) 0
      else scala.math.abs(i),
      g )
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, g) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, g)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, g1) = nonNegativeInt(rng)
    val (d, g2) = double(g1)
    ((i, d), g2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, g1) = double(rng)
    val (i, g2) = nonNegativeInt(g1)
    ((d, i), g2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, g1) = double(rng)
    val (d2, g2) = double(g1)
    val (d3, g3) = double(g2)
    ((d1, d2, d3), g3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (i, g) = nonNegativeInt(rng)
    if (count == 0) (List(), g)
    else (i :: ints(count - 1)(g)._1, g)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a:A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 6.5
  def mdouble: Rand[Double] = {
    map(nonNegativeInt)(_.toDouble / Int.MaxValue)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def main(args: Array[String]): Unit = {
    println("========================================")
    val rng = SimpleRNG(239847923)
    println(mdouble(rng))
  }
}
