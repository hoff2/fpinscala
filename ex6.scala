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

  // "But there are some functions that we can’t very well write in
  //   terms of map and map2. One such function is
  //   nonNegativeLessThan, which generates an integer between 0
  //   (inclusive) and n (exclusive):"
  // Says who?:
  def nonNegativeLessThan(n: Int): Rand[Int] = map(mdouble)(d => (d * n).floor.toInt)

}
