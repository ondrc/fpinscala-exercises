package fpinscala.ch3.datastructures

import scala.annotation.tailrec

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

  // The original apply was not tail recursive - thus unable to test tail recursivity of fold right in exercise 3.10
  // def apply[A](as: A*): List[A] =
  //    if (as.isEmpty) Nil
  //    else Cons(as.head, apply(as.tail: _*))
  def apply[A](as: A*): List[A] = {
    @tailrec
    def loop(acc: List[A], as: Seq[A]): List[A] =
      if (as.isEmpty) acc
      else loop(Cons(as.head, acc), as.tail)
    loop(Nil, as.reverse)
  }

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

  //exercise_3_4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) if n > 0 => drop(t, n-1)
    case Cons(_, _) => l
  }

  //exercise_3_5
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case Cons(_, _) => l
  }

  // exercise_3_6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  //exercise_3_7
  def product_with_foldRight(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)
  // short circuiting not possible, would need extra short circuit condition to fold right - or lazy structure like streams

  //exercise_3_8
  def selfFoldRight = foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))

  //exercise_3_9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, x) => x + 1)

  //exercise_3_10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(acc: B, as: List[A]): B = as match {
      case Nil => acc
      case Cons(h, t) => loop(f(acc, h), t)
    }
    loop(z, as)
  }

  //exercise_3_11
  def lenFL[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)
  def sumFL(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def productFL(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  //exercise_3_12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((acc, h) => Cons(h, acc))

  // exercise_3_13
  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((a, b) => f(b, a))

  // exercise_3_14
  def append[A](l: List[A], e: A): List[A] = foldRight2(l, List(e))(Cons(_, _))

  // exercise_3_15
  def flatten[A](l: List[List[A]]): List[A] = reverse(foldLeft(l, Nil:List[A])((acc, as) => foldLeft(as, acc)((t, h) => Cons(h, t))))

  // exercise_3_16 (page 42)
}
