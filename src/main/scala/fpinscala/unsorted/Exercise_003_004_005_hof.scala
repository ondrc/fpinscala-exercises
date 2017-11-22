package fpinscala.unsorted

object Exercise_003_004_005_hof {

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => (b => f(a,b))

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a,b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def main(args: Array[String]): Unit = {
    val f = (i: Int) => i.toString
    val g = (s: String) => List(s, s)
    println(f andThen g)
    println(g compose f)
  }
}
