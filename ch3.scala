import scala.collection.immutable.{List => _, Nil => _}

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

def tail[A](l: List[A]) = l match {
  case Nil => Nil
  case Cons(h, t) => t
}

def setHead[A](l: List[A], e: A) = l match {
  case Nil => Nil
  case Cons(h, t) => Cons(e, t)
}

def drop[A](n: Int, l: List[A]): List[A] = l match {
  case Nil => Nil
  case Cons(h, t) =>
    if (n <= 0) l
    else drop(n-1, t)
}

def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  case Nil => Nil
  case Cons(h, t) =>
    if (f(h)) t
    else l
}

def init[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case Cons(_, Nil) => l
  case Cons(h, t) => Cons (h, init(t))
}

def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

// 3.9
def length[A](as: List[A]): Int =
  foldRight(as, 0)((_, n) => n + 1)

def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

// replacing foldRight with foldLeft doesn't matter
//def sum(ints: List[Int]): Int = foldRight(ns, 0)((x, y) => x + y)
def sum(ns: List[Int]): Int =
  foldLeft(ns, 0)((x, y) => x + y)

//def product(ints: List[Double]) = foldRight(ns, 1.0)(_ * _)
def product(ns: List[Double]) =
  foldLeft(ns, 1.0)(_ * _)

// except the order of f's params
//def length[A](as: List[A]): Int =
//  foldLeft(as, 0)((n, _) => n + 1)

// 3.12 -- note it's a lot like 3.8
def reverse[A](as: List[A]): List[A] =
  foldLeft(as, Nil: List[A])((t, h) => Cons(h, t))

// i think you can foldRight jjust by foldLefting the reverse etc
// but it feels too easy :D

def append[A](as1: List[A], as2: List[A]) =
  foldRight(as1, as2)(Cons(_, _))

def concat[A](ll: List[List[A]]): List[A] =
  foldRight(ll, Nil:List[A])(append)

def incAll(is: List[Int]) =
  foldRight(is, Nil:List[Int])((h, t) => Cons(h + 1, t))

def dtos(ds: List[Double]): List[String] =
  foldRight(ds, Nil:List[String])((h, t) => Cons(h.toString, t))

def map[A, B](as: List[A])(f: A => B): List[B] =
  foldRight(as, Nil: List[B])((a, bs) => Cons(f(a), bs))

// 3.19
def filter[A](as: List[A])(f: A => Boolean): List[A] =
  foldRight(as, Nil: List[A])((h, t) =>
    if (f(h)) Cons(h, t)
    else t)

def evens(is: List[Int]) = filter(is)(_ % 2 == 0)

def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
  concat(map(as)(f))

def filter2[A](as: List[A])(f: A => Boolean): List[A] =
  foldRight(as, Nil: List[A])((h, t) =>
    if (f(h)) List(h)
    else Nil)

def addzip(l1: List[Int], l2: List[Int]): List[Int] =
  (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) =>
      Cons(h1+h2, addzip(t1, t2))
    case (_,_) => Nil
  }

def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] =
  (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) =>
      Cons(f(h1, h2), zipWith(t1, t2)(f))
    case (_,_) => Nil
  }

def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
  def startsWith[A](l: List[A], p: List[A]): Boolean = (l, p) match {
    case (Cons(lh, lt), Cons(ph, pt)) => lh == ph && startsWith(lt, pt)
    case (_, Nil) => true
    case (_, _) => false
  }
  sup match {
    case Cons(h, t) => startsWith(sup, sub) || hasSubsequence(t, sub)
    case _ => false
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l, r) => 1 + size(l) + size(r)
}

def maximum(t: Tree[Int]): Int = t match {
  case Leaf(v) => v
  case Branch(l, r) => maximum(l) max maximum(r)
}

def depth[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 0
  case Branch(l, r) => 1 + (depth(l) max depth(r))
}

def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
  case Leaf(a) => Leaf(f(a))
  case Branch(l, r) => Branch(map(l)(f), map(r)(f))
}

def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
  case Leaf(a) => f(a)
  case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
}

def sizef[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

def maximumf(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

def depthf[A](t: Tree[A]): Int = fold(t)(_ => 0)(_ max _ + 1)

def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
  fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
