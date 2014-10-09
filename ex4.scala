import scala.{Option => _, Either => _, _}

object Ex4 {

  //=== OPTION ===

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

  object Option {
    // 4.2
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.nonEmpty) Some(xs.sum / xs.size)
      else None

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))

    // 4.3
    // def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    //   case (Some(a), Some(b)) => Some(f(a, b))
    //   case _ => None
    // }

    // def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    //   for {
    //     aa <- a
    //     bb <- b
    //   } yield f(aa, bb)

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a flatMap (aa => b map (bb => f(aa, bb)))

    // 4.4
    // def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    //   case Nil => Some(Nil)
    //   case (None :: t) => None
    //   case (Some(v) :: t) => sequence(t).map{ tt => v :: tt }
    // }
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

    // 4.5 (wrong)
    // def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    //   case Nil => Some(Nil)
    //   case h :: t => f(h) flatMap(hh => sequence(t)(f) map (hh :: _))
    // }
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

    def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity _)
  }

  //=== EITHER ===

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
        Either[EE, C] = for {
      aa <- this
      bb <- b
    } yield(f(aa, bb))
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    // def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    //   case Nil => Right(Nil)
    //   case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    // }

    // def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    //   case Nil => Right(Nil)
    //   case h :: t => h flatMap (hh => traverse(t)(f) map (hh :: _))
    // }
  }

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}
