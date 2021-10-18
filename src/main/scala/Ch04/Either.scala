package Ch04

sealed trait Either[+E, +A] {
  // 4-6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(v) => Left(v)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(v) => Left(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(v) => Right(v)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)

  def map21[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap(aa => b.map(bb => f(aa, bb)))

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean og empty list")
    else
      Right(xs.sum / xs.length)

  // 4-7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case x :: xs => for {
      xx <- x
      xxs <- sequence(xs)
    } yield xx :: xxs
    case Nil => Right(Nil)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case x :: xs => for {
      xx <- f(x)
      xxs <- traverse(xs)(f)
    } yield xx :: xxs
    case Nil => Right(Nil)
  }
}

case class Person(name: Name, age: Age)

sealed class Name(val value: String)

sealed class Age(val value: Int)

object Person {

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] = for {
    n <- mkName(name)
    a <- mkAge(age)
  } yield Person(n, a)

  // 4-8
}
