package fpinscala.ch3.datastructures

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fpinscala.ch3.datastructures.List._

@RunWith(classOf[JUnitRunner])
class ListSuite extends FunSuite {

  // TODO - add tests of other exercises too

  test("pattern matching") {
    assert(List.x === 3)
  }

  test("length") {
    assert(length(List(1, 2, 3)) === 3)
  }

  test("apply") {
    assert(List(1, 2, 3) === Cons(1, Cons(2, Cons(3, Nil))))
  }

  test("foldRight not tail recursive") {
    intercept[StackOverflowError] {
      foldRight(List[Int](Stream.fill(10000)(1):_*), 0)(_ + _)
    }
  }

  test("foldLeft is tail recursive") {
    assert(foldLeft(List[Int](Stream.fill(10000)(1):_*), 0)(_ + _) === 10000)
  }

  test("lenFL") {
    assert(lenFL(List(1, 2, 3, 4, 5)) == 5)
  }

  test("lenFL empty") {
    assert(lenFL(List()) == 0)
  }

  test("sumFL") {
    assert(sumFL(List(1, 2, 3)) == 6)
  }

  test("sumFL empty") {
    assert(sumFL(List()) == 0)
  }

  test("productFL") {
    assert(productFL(List(1.0, 2.0, 3.0)) == 6.0)
  }

  test("productFL empty") {
    assert(productFL(List()) == 1.0)
  }

  test("reverse List(1, 2, 3) == List(3, 2, 1)") {
    assert(reverse(List(1, 2, 3)) === List(3, 2, 1))
  }

  test("foldRight2 List(1, 2, 3), 0, _ -_") {
    assert(foldRight2(List(1, 2, 3), 0)(_ - _) === foldRight(List(1, 2, 3), 0)(_ - _))
  }

  test("append List(1, 2, 3) 4 is List(1, 2, 3, 4)") {
    assert(append(List(1, 2, 3), 4) === List(1, 2, 3, 4))
  }


  test("flatten List(List(1, 2), List(3, 4), List(5, 6)) ia List(1, 2, 3, 4, 5, 6)") {
    assert(flatten(List(List(1, 2), List(3, 4), List(5, 6))) === List(1, 2, 3, 4, 5, 6))
  }
}
