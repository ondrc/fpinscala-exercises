package fpinscala.unsorted

object Exercise001_Fib {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(i: Int, ni: Int, nii: Int): Int = i match {
      case 0 => ni + nii
      case i => loop(i-1, nii, ni + nii)
    }
    n match {
      case 1 => 0
      case 2 => 1
      case i if i > 2 => loop(i - 3, 0, 1)
      case _ => throw new IllegalArgumentException
    }
  }

  def main(arg: Array[String]): Unit = {
    print(
      1.until(10).map(fib)
    )
  }
}


