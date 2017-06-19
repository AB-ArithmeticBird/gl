package goodlord
import org.scalatest.FunSuite

/**
  * Created by arithmeticbird on 19/6/17.
  */
class TestSuite extends FunSuite {
  test("Lcs works for given test") {
    val result = GoodLordTest1.lcs("ABCD", "CDRT")
    assert(result == "CD")
  }
  test("Lcs works for empty string") {
    val result = GoodLordTest1.lcs("", "")
    assert(result == "")
  }
}
