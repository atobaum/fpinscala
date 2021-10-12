package Ch04

sealed trait Option[+A] {
  // 4-1
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case _ => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    if (this == None) ob
    else this
  }

  def orElse2[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => _
  }

  def filter(f: A => Boolean): Option[A] = {
    if (this.map(f) == None) None
    else this
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  // 4-2
  def div(numerator: Double, denominator: Double): Option[Double] = denominator match {
    case 0 => None
    case v => Some(numerator / v)
  }

  def avg(xs: Seq[Double]): Option[Double] = div(xs.sum, xs.length)

  def square(x: Double): Double = x * x

  def variance(xs: Seq[Double]): Option[Double] =
    avg(xs).
      map(avg => xs.map(x => math.pow(x - avg, 2))).
      flatMap(avg)

  def variance2(xs: Seq[Double]): Option[Double] =
    avg(xs.map(square)).flatMap(a => avg(xs).map(square).map(b => a - b))

  // 4-3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a => b.map(b => f(a, b)))

  // 4-4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case x :: xs => map2(x, sequence(xs))(_ :: _)
    case Nil => Some(Nil)
  }

  // 4-5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)
    case Nil => Some(Nil)
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

  // in book
  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

}
