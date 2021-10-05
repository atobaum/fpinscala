import scala.annotation.tailrec

object Ex2 {
  // 2-1
  def fib(n: Int): Int = {
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)
  }

  // 2-2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)

    loop(0)
  }

  // 2-3
  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  // 2-4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // 2-5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]): Unit = {
    println(compose((a: Int) => a * 3, (a: Int) => a + 2)(2))
  }
}