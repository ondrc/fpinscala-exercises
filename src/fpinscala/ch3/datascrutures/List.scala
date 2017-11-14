package fpinscala.ch3.datascrutures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //exercise_3_1
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  //exercise_3_2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil // we chose to return Nil, we might want to raise an error
    case Cons(_, t) => t
  }

  //exercise_3_3
  def setHead[A](l: List[A], h: A) = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }
}
