package fpinscala.errorhandling

import scala.{Option => _, Either => _, _}

object Ch4 {

  object Option {
    sealed trait Option[+A] {
      def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
      }

      def flatMap[B](f: A => Option[B]): Option[B] =
        this.map(f).getOrElse(None)

      def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(b) => b
      }

      def orElse[B >: A](ob: => Option[B]): Option[B] =
        this.map(Some(_)).getOrElse(ob)

      def filter(f: A => Boolean): Option[A] =
        this.flatMap(a => if (f(a)) Some(a) else None)
    }

    case class Some[+A](get: A) extends Option[A]
    case object None extends Option[Nothing]

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

    def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
      oa.flatMap(a => ob.map(b => f(a, b)))

    // def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    //   case (Some(a), Some(b)) => Some(f(a, b))
    //   case _ => None
    // }

    // def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    //   for {
    //     aa <- a
    //     bb <- b
    //   } yield f(aa, bb)

    // i gave up on this one after a good fight
    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x,y)(_ :: _))

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)(_ :: _))
  }

  object Either {

    sealed trait Either[+E, +A] {
      def map[B](f: A => B): Either[E, B] =
        this match {
          case Left(e) => Left(e)
          case Right(a) => Right(f(a))
        }

      def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
        this match {
          case Left(e) => Left(e)
          case Right(a) => f(a)
        }

      def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
        this match {
          case Left(_) => b
          case Right(b) => Right(a) // or even `this`
        }

      def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
        for {
          a <- this
          b1 <- b
        } yield f(a, b1)
        // (this, b) match {
        //   case (Right(a), Right(b)) => Right(f(a,b))
        //   case (Right(a), Left(e)) => Left(e)
        //   case (Left(e), _) => Left(e)
        // }
    }
    case class Left[+E](value: E) extends Either[E, Nothing]
    case class Right[+A](value: A) extends Either[Nothing, A]

  }
}
