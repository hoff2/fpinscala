object ex6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

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
    val (nn, nextRNG) = rng.nextInt
    val n = if (nn < 0) -(nn + 1) else nn
    //val n = if (nn < -Int.MaxValue) -nn else Int.MaxValue
    (n, nextRNG)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (nn, nextRNG) = nonNegativeInt(rng)
    val d =  nn / (Int.MaxValue.toDouble+1)
    (d, nextRNG)
  }

  // def boolean(rng: RNG): (Boolean, RNG) = {
  //   val (nn, nextRNG) = rng.nextInt
  //   (nn < 0, nextRNG)
  // }

  // 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng2) = double(rng)
    val (n, rng3) = rng2.nextInt
    ((d, n), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (List[Int](), rng)
    else {
      val (n, nrng)  = rng.nextInt
      val (ns, frng) = ints(count - 1)(nrng)
      (n :: ns, frng)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  // 6.5
  def mdouble: Rand[Double] = map(nonNegativeInt)(n => n / (Int.MaxValue.toDouble+1))

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  // 6.7
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_,_))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((hr, tr) => map2(hr, tr)(_::_))

  def _ints(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  // "But there are some functions that we can’t very well write in
  //   terms of map and map2. One such function is
  //   nonNegativeLessThan, which generates an integer between 0
  //   (inclusive) and n (exclusive):"
  // Says who?:
  def _nonNegativeLessThan(n: Int): Rand[Int] = map(mdouble)(d => (d * n).floor.toInt)

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  // 6.8 :[
  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    f(a)(rng2)
  }

  // 6.9 :[
  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => _map(rb)(b => f(a, b)))
}



object ex66 {

  type Rand[A] = State[ex6.RNG, A]

  import State._

  // 6.10 :[
  case class State[S, +A](run: S => (A, S)) {

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
              val (a, ns) = run(s)
              f(a).run(ns)
            })

    def map[B](f: A => B): State[S, B] =
     flatMap(a => unit(f(a)))

    def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      fs.foldRight(unit[S, List[A]](List()))((hr, tr) => hr.map2(tr)(_::_))
    //                 ^^^^^^^^^^^^ how we were supposed to know to put this here,
    //                              i have no idea

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get // Gets the current state and assigns it to `s`.
      _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
    } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

  // 6.11
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  //            I see what you did there ---VVVVVVVVVVVVV
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State.sequence(
      inputs.map(input => modify[Machine](machine => machine)))

  }
}
