object Ch2 {
  object Ex1 {
    def fib(n: Int): Int = {
      if (n <= 0) 0
      else if (n == 1) 1
      else fib(n-1) + (n-2)
    }
  }

  object Ex2 {
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
      @annotation.tailrec
      def go(n: Int): Boolean =
        if (n >= as.length - 1) true
        else if (!ordered(as(n), as (n+1))) false
        else go(n+1)
      go(0)
    }
  }

  object Ex3and4and5 {
    def curry[A, B, C](f:(A, B)=>C) : A => (B => C) =
      (a) => ((b) => f(a, b))

    def uncurry[A, B, C](f: A => B => C): (A, B) => C =
      (a, b) => f(a).apply(b)


    def compose[A, B, C](f: B=>C , g: A=>B): A => C =
      (a: A) => f(g(a))
  }
}


