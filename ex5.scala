object ex5 {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    // 5.1
    def toList: List[A] = this match {
      case Empty => List()
      case Cons(h, t) => h() +: t().toList
    }

    // 5.2
    def take(n: Int): Stream[A] = (this, n) match {
      case (_, 0)          => Empty
      case (Empty, _)      => Empty //error("ran out of stream!")
      case (Cons(h, t), _) => Cons(h, () => t().take(n-1))
    }

    def drop(n: Int): Stream[A] = (this, n) match {
      case (_, 0)          => this
      case (Empty, _)      => this //error("ran out of stream!")
      case (Cons(h, t), _) => t().drop(n-1)
    }

    // 5.3
    def takeWhile0(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if (p(h())) => Cons(h, () => t().takeWhile0(p))
      case _ => Empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    // 5.4
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    // 5.5
    def takeWhile(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((a, b) =>
        if (p(a)) Cons(() => a, () => b.takeWhile(p))
        else Empty)

    // 5.6
    def headOption0: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
  
    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

}
