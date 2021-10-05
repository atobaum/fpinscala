sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  // 3-2
  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case _ => Nil
  }

  // 3-3
  def setHead[A](l: List[A], v: A): List[A] = l match {
    case Cons(_, t) => Cons(v, t)
    case _ => Nil
  }

  // 3-4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Cons(_, t) => drop(t, n - 1)
      case _ => Nil
    }
  }

  // 3-5
  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
    case Cons(h, t) =>
      if (p(h)) dropWhile(t, p)
      else l
    case _ => Nil
  }

  // 3-6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Cons(x, xs) => foldRight(xs, f(x, z))(f)
    case Nil => z
  }
}

object Ex3 {
  // 3-1 answer: 3번째에 걸려서 3

  // 3-2
}
