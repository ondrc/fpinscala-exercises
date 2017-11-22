package fpinscala.ch3.datastructures

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fpinscala.ch3.datastructures.List._

@RunWith(classOf[JUnitRunner])
class ListSuite extends FunSuite {

  // TODO - add tests of previous exercises too

  test("length") {
    assert(length(List(1, 2, 3)) === 3)
  }
}
