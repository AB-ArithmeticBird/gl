import Test2._
import org.scalatest.FunSuite

class ExpressionTestSuite extends FunSuite {

  test("An expression will not evaluate if first number is greater than second operand in Mul") {
    val ex = Other(Mul, Number(5), Other(Add, Number(1), Number(2)))
    assert(eval(ex) == List())
  }

  test("An expression is evaluated correctly with Mul if first operand is less then the second") {
    val ex = Other(Mul, Number(2), Other(Add, Number(3), Number(4)))
    assert(eval(ex) == List(14))
  }

  test("An expression is not evaluated if first operand is 1 in Mul") {
    val ex = Other(Mul, Number(1), Other(Add, Number(3), Number(4)))
    assert(eval(ex) == List())
  }

  test("An expression is not evaluated if first operand is less than the second in Sub") {
    val ex = Other(Mul, Number(2), Other(Sub, Number(3), Number(6)))
    assert(eval(ex) == List())
  }

  test("An expression is not evaluated if any operand in Mul is 1") {
    val ex = Other(Mul, Number(2), Other(Sub, Number(4), Number(3)))
    assert(eval(ex) == List())
  }

  test("An expression is evaluated correctly if first operand > second oprand in sub") {
    val ex = Other(Mul, Number(2), Other(Sub, Number(8), Number(5)))
    assert(eval(ex) == List(6))
  }

  test("An expression is not evaluated if first operand > second in Add") {
    val ex = Other(Mul, Number(2), Other(Add, Number(2), Number(1)))
    assert(eval(ex) == List())
  }

  test("subsequence of a list") {
    val ex = combinations(List(1, 2, 3))
    assert(ex.size == 8)
  }

  test("choices of a list") {
    val ex = choices(List(1, 2))
    assert(ex.size == 5)
  }
  test("split returns all possible ways of splitting a list into two non-empty lists") {
    val ex = split(List(1, 2, 3))
    assert(ex.size == 2)
    assert(ex(0) == (List(1), List(2, 3)))
    assert(ex(1) == (List(1, 2), List(3)))
  }
}
