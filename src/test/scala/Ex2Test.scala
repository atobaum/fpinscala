import org.scalatest.funspec.AnyFunSpec

class Ex2Test extends AnyFunSpec {
  describe("fib") {
    it("fib(0) = 1") {
      assert(Ex2.fib(0) == 0)
    }
    it("fib(5) = 120") {
      assert(Ex2.fib(12) == 144)
    }
  }

  describe("isSorted") {
    it("for string array") {
      assert(Ex2.isSorted(Array("aaa", "bbb", "ccc"), (a: String, b: String) => a < b))
      assert(!Ex2.isSorted(Array("aaa", "ccc", "bbb"), (a: String, b: String) => a < b))
    }

    it("for int array") {
      assert(Ex2.isSorted(Array(1, 4, 6), (a: Int, b: Int) => a < b))
      assert(!Ex2.isSorted(Array(1, -4, 5), (a: Int, b: Int) => a < b))
    }
  }

  it("curry, uncurry") {
    assert(Ex2.curry((a: Int, b: Int) => a + b)(2)(3) == 5)
    assert(Ex2.uncurry((a: Int) => (b: Int) => a + b)(2, 3) == 5)
  }

  it("compose"){
    assert(Ex2.compose((a: Int) => a * 3, (a: Int) => a + 2)(2) == 12)
  }
}
