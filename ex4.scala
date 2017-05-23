// object Ch4 {

//   def sequenceC[A](a: List[Option[A]]): Option[List[A]] = a match {
//     case Nil => Some(Nil)
//       case (None :: t) => None
//       case (Some(v) :: t) => sequence(t).map{ tt => v :: tt }
//     }

//     def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
//       case Nil => Some(Nil)
//       case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
//     }

//     // 4.5 (wrong)
//     // def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
//     //   case Nil => Some(Nil)
//     //   case h :: t => f(h) flatMap(hh => sequence(t)(f) map (hh :: _))
//     // }
//     def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
//       a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))

//     def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity _)
//   }

//   //=== EITHER ===

//   sealed trait Either[+E, +A] {
//     def map[B](f: A => B): Either[E, B] = this match {
//       case Left(e) => Left(e)
//       case Right(a) => Right(f(a))
//     }

//     def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
//       case Left(e) => Left(e)
//       case Right(a) => f(a)
//     }

//     def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
//       case Left(_) => b
//       case Right(a) => Right(a)
//     }

//     def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
//         Either[EE, C] = for {
//       aa <- this
//       bb <- b
//     } yield(f(aa, bb))
//   }

//   case class Left[+E](value: E) extends Either[E, Nothing]
//   case class Right[+A](value: A) extends Either[Nothing, A]

//   object Either {
//     // def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
//     //   case Nil => Right(Nil)
//     //   case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
//     // }

//     // def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
//     //   case Nil => Right(Nil)
//     //   case h :: t => h flatMap (hh => traverse(t)(f) map (hh :: _))
//     // }
//   }

//   case class Person(name: Name, age: Age)
//   sealed class Name(val value: String)
//   sealed class Age(val value: Int)

//   def mkName(name: String): Either[String, Name] =
//     if (name == "" || name == null) Left("Name is empty.")
//     else Right(new Name(name))

//   def mkAge(age: Int): Either[String, Age] =
//     if (age < 0) Left("Age is out of range.")
//     else Right(new Age(age))

//   def mkPerson(name: String, age: Int): Either[String, Person] =
//     mkName(name).map2(mkAge(age))(Person(_, _))
// }
