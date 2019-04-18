package user

import homegrown.collections._

class FoldableSuite extends TestSuite {
  test("pretending to test contains") {
    Cell(1).contains(123) shouldBe false
    Cell(1).contains(1) shouldBe true
  }
}

case class Cell(input: Int) extends Foldable[Int] {
  final override def foldLeft[Result](seed: Result)(function: (Result, Int) => Result): Result =
    function(seed, input)

  final override def foldRight[Result](seed: => Result)(function: (Int, => Result) => Result): Result =
    function(input, seed)
}
