import scala.annotation.tailrec

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

  // 3-7
  // ㄴㄴ Nil 이 아니가 때문에 foldRight를 계속 후촐한다.
  // f 가 0을 가지고 무얼할지 모르기 때문에

  // 3-8
  // foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _) = List(1,2,3) 일 듯.
  // 답지 왈: we can think foldRight as replacing Const with f and Nil with z.

  // 3-9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, l) => l + 1)

  // 3-10
  // foldLeft는 방향이 반대: 좌 => 우
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // 3-11
  def sumLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def productLeft(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  // 3-12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, h) => Cons(h, acc))

  // 3-13
  // TODO: 풀기
  //  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =

  // 3-14
  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))

  def append2[A]: (List[A], List[A]) => List[A] = foldRight(_, _)(Cons(_, _))

  // 3-15
  def flat[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil: List[A])(append)

  // 3-16
  def add1: List[Int] => List[Int] =
    foldRight(_, Nil: List[Int])((x, xs) => Cons(x + 1, xs))

  // 3-17
  def doubleToStringList: List[Double] => List[String] =
    foldRight(_, Nil: List[String])((h, acc) => Cons(h.toString, acc))

  // 3-18
  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

  // 3-19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) Cons(x, filter(xs)(f))
      else filter(xs)(f)
  }

  def removeOdds: List[Int] => List[Int] =
    filter(_)(_ % 2 == 0)

  // 3-20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flat(map(as)(f))

  // 3-21
  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x=>if(f(x)) List(x) else List() )

  // 3-22
  def addPointwise(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Cons(lx, lxs), Cons(rx, rxs)) => Cons(lx + rx, addPointwise(lxs, rxs))
    case _ => Nil
  }

  // 3-23
  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r)match {
    case (Cons(lx, lxs), Cons(rx, rxs)) => Cons(f(lx, rx), zipWith(lxs, rxs)(f))
    case _ => Nil
  }

  // 3-24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(lx, lxs), Cons(rx, rxs)) =>
      if (lx == rx) hasSubsequence(lxs, rxs) || hasSubsequence(lxs, sub)
      else hasSubsequence(lxs, sub)
    case (_, Nil) => true
    case (Nil, _) => false
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    case Nil => z
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{
  // 3-25
  def size[A](t: Tree[A]): Int = t match {
    case Branch(l, r) => size(l) + size(r) + 1
    case _ => 1
  }

  // 3-26
  def maximum: Tree[Int] => Int = {
    case Branch(l, r) => maximum(l).max(maximum(r))
    case Leaf(v) => v
  }

  // 3-27
  def depth[A]: Tree[A] => Int = {
    case Branch(l, r) => depth(l).max(depth(r)) + 1
    case _ => 1
  }

  // 3-28
  def map[A, B](t: Tree[A])(f: A=>B): Tree[B] = t match {
    case Branch(l ,r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v) => Leaf(f(v))
  }

  // 3-29
  // TODO: 풀기
//  def fold[A, B](t: Tree[A])(f: (A, B)=> B):B = t match{
//    case Branch(l ,r) => Branch(map(l)(f), map(r)(f))
//    case Leaf(v) => Leaf(f(v))
//  }
}

object Ex3 {
  // 3-1 answer: 3번째에 걸려서 3

  // 3-2
}
