package fpinscala.unsorted

object Exercise_002_IsSorted {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (ordered(as(n-1), as(n))) loop(n+1)
      else false
    loop(1)
  }

  def main(arg: Array[String]): Unit = {
    println(
      isSorted(Array(1, 2, 3), (a: Int, b: Int) => a <= b)
    )
    println(
      isSorted(Array(1, 2, 2, 3), (a: Int, b: Int) => a <= b)
    )
    println(
      isSorted(Array(1, 2, 2, 1), (a: Int, b: Int) => a <= b)
    )
    println(
      isSorted(Array(), (a: Int, b: Int) => a <= b)
    )
    println(
      isSorted(Array(0), (a: Int, b: Int) => a <= b)
    )
  }
}
