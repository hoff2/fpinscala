import scala.{Option => _, Either => _, _}

object Ex4 {

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  // 4.1
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match{
      case None    => None
      case Some(a) => f(a)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None    => default
      case Some(a) => a
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None    => ob
      case Some(a) => Some(a) // thought about "this", wasn't sure
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => Some(a) //likewise
      case _ => None
    }
  }

  // 4.2
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.nonEmpty) Some(xs.sum / xs.size)
    else None

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))

  // 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }

}
