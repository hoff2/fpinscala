//package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail:List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil //(or error?)
    case Cons(x, xs) => xs
  }

  def setHead[A](as: List[A], nh: A): List[A] = Cons(nh, tail(as))

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => if (n == 0) l
                       else drop(t, n - 1)
  }

  // // drop using tail
  // def drop2[A](l: List[A], n: Int): List[A] =
  //   if (n == 0) l
  //   else drop2(tail(l), n - 1)

  // // tail using drop
  // def tail2[A](as: List[A]): List[A] = drop(as, 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f)
                       else l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(lh, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
    case Nil => Nil // but wrong :D
  }

  // def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
  //   case Nil => z
  //   case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  // }

  // foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))
  //                             ^^ why is this needed and what is it saying?

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, l) => l + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length2[A](as: List[A]): Int = foldLeft(as, 0)((l, _) => l + 1)

  def reverse[A](as: List[A]) =
    foldLeft(as, Nil: List[A])((acc, a) => Cons(a, acc))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a, acc) => Cons(a, acc))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append2) // i originally had (a, acc) => append2(a, acc)

  // def incrementAll(l: List[Int]): List[Int] = l match {
  //   case Nil => Nil
  //   case Cons(x, xs) => Cons(x+1, incrementAll(xs))
  // }

  // def stringifyDoubles(l: List[Double]): List[String] = l match {
  //   case Nil => Nil
  //   case Cons(x, xs) => Cons(x.toString, stringifyDoubles(xs))
  // }

  def incrementAll(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def stringifyDoubles(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, t) => append(f(h), t))

//  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addPairwise(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
  }

  def beginsWith[A](l: List[A], q: List[A]): Boolean = (l, q) match {
    case (_, Nil) => true
    case (Cons(lh, lt), Cons(qh, qt)) if lh == qh => beginsWith(lt, qt)
    case _ => false
  }

  def hasSubsequence[A](l: List[A], q: List[A]): Boolean = (l, q) match {
    case (Nil, _) => false
    case (Cons(_, lt), _) => beginsWith(l, q) || hasSubsequence(lt, q)
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ +_ + 1)

  def maximum2(t: Tree[Int]): Int = fold(t)(v => v)(_ max _)

  def depth2[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((ld, rd) => 1 + (ld max rd))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_,_))
}
