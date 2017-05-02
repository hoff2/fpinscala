import scala.{Option => _, Some => _, Either => _, _}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  //    def flatMap[B](f: A => Option[B]): Option[B] = this match {
  //      case None => None
  //      case Some(a) => f(a)
  //    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(b) => b
  }

  //    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
  //      case None => ob
  //      case _ => this
  //    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)

  //    def filter(f: A => Boolean): Option[A] = this match {
  //      case None => None
  //      case Some(a) => if (f(a)) this else None
  //    }

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(a => if (f(a)) Some(a) else None)
}

def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
  oa.flatMap(a => ob.map(b => f(a, b)))

//scala> Ch4.map2(Ch4.None, Ch4.Some(3))((a: Int, b: Int) => a + b)
//res6: Ch4.Option[Int] = None

// scala> Ch4.map2(Ch4.Some(1), Ch4.None)((a: Int, b: Int) => a+b)
// res2: Ch4.Option[Int] = None

// scala> Ch4.map2(Ch4.Some(1), Ch4.Some(3))((a: Int, b: Int) => a+b)
// res3: Ch4.Option[Int] = Some(4)

// scala> Ch4.map2(Ch4.Some(1), Ch4.Some(2))(_ + _)
// res0: Ch4.Option[Int] = Some(3)

// scala> Ch4.map2(Ch4.Some(1), Ch4.None)(_ + _)
//   <console>:22: error: ambiguous reference to overloaded definition,
// both method + in class Int of type (x: Char)Int
// and  method + in class Int of type (x: Byte)Int
//   match argument types (Nothing)
// Ch4.map2(Ch4.Some(1), Ch4.None)(_ + _)

// scala> Ch4.map2(Ch4.None, Ch4.Some(3))(_ + _)
//   <console>:22: error: value + is not a member of Nothing
// Ch4.map2(Ch4.None, Ch4.Some(3))(_ + _)
