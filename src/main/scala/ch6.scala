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

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng_) = s(rng)
      (f(a), rng_)
    }

  // 6.5
  def mdouble: Rand[Double] = {
    map(nonNegativeInt)(_.toDouble / Int.MaxValue)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng0 => {
      val (a, rng1) = ra(rng0)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil: List[A]))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng0 => {
      val (a, rng1) = f(rng0)
      g(a)(rng1)
    }

  // def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
  //   flatMap(nonNegativeInt(rng))
  // }

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => _map(rb)(b => f(a, b)))


  case class State[S, +A](run: S => (A, S)) {

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s0 => {
        val (a, s1) = run(s0)
        f(a).run(s1)
      })

    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))
  }

  object State {

    def unit[S, A](a: A): State[S, A] = State(state => (a, state))

    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
      sas.reverse.foldLeft(
        unit[S, List[A]](List()))(
        (acc, sa) => sa.map2(acc)(_ :: _))
  }

  // ========================================

  // def main(args: Array[String]): Unit = {
  //   println("========================================")
  //   val rng = SimpleRNG(239847923)
  //   println(mdouble(rng))
  // }
}

object SixEleven {

  import Ch6.State

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
      inputs match {
        case (i :: is) => State((m: Machine) => ((m.candies - 1, m.coins + 1), this))
        case _ => State.unit((candies, coins))
      }

    def interact(input: Input): Machine = (this, input) match {
      case (Machine(_, 0, _), _) => this
      case (Machine(true, a, o), Turn) => this
      case (Machine(false, a, o), Coin) => this
      case (Machine(true, a, o), Coin) => Machine(false, a, o)
      case (Machine(false, a, o), Turn) => Machine(true, a - 1, o + 1)
    }
  }

  // def main(args: Array[String]): Unit = {
  //   println("========================================")
}
