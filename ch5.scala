//package fpinscala.laziness

object Ch5 {

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

    val ones: Stream[Int] = Stream.cons(1, ones)

    def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

    def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

    def fibs: Stream[Int] = {
      def from(f0: Int, f1: Int): Stream[Int] = Stream.cons(f0, from(f1, f0 + f1))
      from(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case None => Empty
        case Some((a, s)) => Stream.cons(a, unfold(z)(f))
      }

    def uconstant[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

    def ufrom(n: Int): Stream[Int] = unfold(n)(p => Some(p, p+1))

    def ufibs: Stream[Int] = unfold((0, 1)){
      case (f0, f1) => Some((f0, (f1, f0 + f1)))
    }

    def uOnes: Stream[Int] = uconstant(1)
  }

  sealed trait Stream[+A] {

    import Ch5.Stream._

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def toList: List[A] = this match {
      case Empty => List()
      case Cons(h, t) => h() +: t().toList
    }

    def take(n: Int): Stream[A] = (this, n) match {
      case (Cons(h, t), n) if (n > 0) => Cons(h, () => t().take(n - 1))
      case _ => Empty
    }

    def drop(n: Int): Stream[A] = (this, n) match {
      case (Cons(h, t), n) if (n > 0) => t().drop(n - 1)
      case _ => this
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if (p(h())) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile1(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((a, b) =>
        if (p(a)) Cons(() => a, () => b)
        else Empty)

    def headOption1: Option[A] =
      foldRight(None: Option[A])((a, b) => Some(a))

    def map[B](f: A => B): Stream[B] =
      foldRight(Empty: Stream[B])((a, b) =>
        Cons(() => f(a), () => b))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((a, b) =>
        if (p(a)) Cons(() => a, () => b)
        else b)

    def append[B>:A](c: => Stream[B]): Stream[B] =
      foldRight(c)((a, b) => Cons(() => a, () => b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

    def umap[B](f: A => B): Stream[B] = unfold(this){
      case Cons(h, t) => Some((f(h()), t()))
      case _          => None
    }

    def utake(n: Int): Stream[A] = unfold(this, n){
      case (_, 0) | (Empty, _) => None
      case (Cons(h, t), n) => Some((h(), (t(), n - 1)))
    }

    def uTakeWhile(f: A => Boolean): Stream[A] = unfold(this){
      case Cons(h, t) if (f(h())) => Some((h(), t()))
      case _ => None
    }

    def uZipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s)){
      case (Empty, _) | (_, Empty) => None
      case (Cons(ah, at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
    }

  }

  // =========================================

  def main(args: Array[String]):Unit = {
    val a = Ch5.Stream.apply(1, 2, 3, 4, 5, 6, 7)
    println(a.takeWhile(_ < 6).toList)
  }
}












//     // i misunderstood the meaning of "with" here (didn't refer back to ch. 3)
//     def uzipWith[B](those: Stream[B]): Stream[(A, B)] = unfold((this, those)){
//       case (Empty, _) | (_, Empty) => None
//       case (Cons(ah, at), Cons(bh, bt)) => Some((ah(), bh()), (at(), bt()))
//     }

//     def uzipAll[B](those: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, those)){
//       case (Empty, Empty)               => None
//       case (Empty, Cons(bh, bt))        => Some((None,       Some(bh())), (Empty, bt()))
//       case (Cons(ah, at), Empty)        => Some((Some(ah()), None),       (at(), Empty))
//       case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
//     }

//     // 5.14
//     def startsWith[A](what: Stream[A]): Boolean = (this, what) match {
//       case (_, Empty) => true
//       case (Empty, _) => false
//       case (Cons(h1, t1), Cons(h2, t2)) => (h1() == h2()) && t1().startsWith(t2())
//     }

//     // 5.15
//     def tails: Stream[Stream[A]] = unfold(this){
//       case Empty        => None
//       case (Cons(_, t)) => Some((t(), t()))
//     }

//     def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

//     // 5.16
//     // did anyone figure this out? I don't think this solution is linear time
//     // as required, even if it did compile
//     // def scanRight(z: => A)(f: (A, => A) => A): Stream[A] = unfold(this){
//     //   case Empty        => None
//     //   case (Cons(_, t)) => Some((t().foldRight(z)(f), t()))
//     // }
//   }

//   case object Empty extends Stream[Nothing]
//   case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

//   object Stream {
//     def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
//       lazy val head = hd
//       lazy val tail = tl
//       Cons(() => head, () => tail)
//     }

//     def empty[A]: Stream[A] = Empty

//     def apply[A](as: A*): Stream[A] =
//       if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

//     // 5.8
//     def constant[A](a: A): Stream[A] = cons(a, constant(a))

//     // 5.9
//     def from(n: Int): Stream[Int] = cons(n, from(n + 1))

//     // 5.10
//     def fibs: Stream[Int] = {
//       def fibsFrom(n1: Int, n2: Int): Stream[Int] =
//         cons(n1, fibsFrom(n2, n1+n2))
//       fibsFrom(0, 1)
//     }

//     // 5.11
//     def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
//       case None => empty
//       case Some((a, s)) => cons(a, unfold(s)(f))
//     }

//     // 5.12
//     def ufibs: Stream[Int] = Stream.unfold((0, 1))(s => Some(s._1, (s._2, s._1+s._2)))

//     def uconstant[A](a: A): Stream[A] = unfold(a)(s => Some((s, s)))

//     def ufrom(n: Int): Stream[Int] = unfold(n)(s => Some((s, s+1)))

//     def uones = uconstant(1)
//   }
// }
