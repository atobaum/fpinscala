import org.scalatest.funspec.AnyFunSpec

class Ex3Test extends AnyFunSpec {
  describe("tail") {
    it("should return tail") {
      assert(List.tail(List(1, 2, 3, 4)) == Cons(2, Cons(3, Cons(4, Nil))))
    }

    it("should return Nil for list of length 1") {
      assert(List.tail(List(1)) == Nil)
    }

    it("should return Nil for Nil") {
      assert(List.tail(Nil) == Nil)
    }
  }

  it("setHead") {
    assert(List.setHead(List(1, 2, 3), 6) == List(6, 2, 3))
  }

  it("drop") {
    assert(List.drop(Nil, 3) == Nil)
    assert(List.drop(List(1, 2, 3, 4), 2) == List(3, 4))
  }

  it("dropWhile") {
    assert(List.dropWhile(Nil, (a: Int) => a < 2) == Nil)
    assert(List.dropWhile(List(1, 2, 3, 4), (a: Int) => a < 2) == List(2, 3, 4))
    assert(List.dropWhile(List(1, 2, 3, 4), (a: Int) => a < 100) == Nil)
  }

  it("init") {
    assert(List.init(Nil) == Nil)
    assert(List.init(List(1)) == Nil)
    assert(List.init(List(1, 2, 3)) == List(1, 2))
  }
}
