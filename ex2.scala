object Ex2 {
def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(n: Int): Boolean = (n >= as.length-1) || (gt(as(n+1), as(n)) && loop(n+1))
  loop(0)
}

@annotation.tailrec
def isSorted2[A](as: Array[A], gt: (A, A) => Boolean, n: Int = 0): Boolean =
  (n >= as.length-1) || (gt(as(n+1), as(n)) && isSorted2(as, gt, n+1))



def   curry[A, B, C](f: (A,   B) => C):  A => (B  => C) =  a => b  => f(a, b)

def uncurry[A, B, C](f:  A => B  => C): (A,    B) => C  = (a,   b) => f(a)(b)



def compose[A, B, C](f: B => C, g: A => B): A => C = a => (f(g(a)))
}
