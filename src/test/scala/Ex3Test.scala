import org.scalatest.funspec.AnyFunSpec

class Ex3Test extends AnyFunSpec {
  describe("List") {
    it("should return tail") {
      assert(List.tail(List(1, 2, 3, 4)) == Cons(2, Cons(3, Cons(4, Nil))))
    }

    it("should return Nil for list of length 1") {
      assert(List.tail(List(1)) == Nil)
    }

    it("should return Nil for Nil") {
      assert(List.tail(Nil) == Nil)
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

    it("3-8") {
      assert(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3))
    }

    it("length") {
      assert(List.length(Nil) == 0)
      assert(List.length(List(1, 2)) == 2)
      assert(List.length(List(1, 2, 3, 4, 5)) == 5)
    }

    it("foldLeft") {
      assert(List.foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) == 15)
      assert(List.foldLeft(List("a", "b", "c"), "")(_ + _) == "abc")
    }

    it("sumLeft") {
      assert(List.sumLeft(List(1, 2, 3, 4, 5)) == 15)
    }

    it("productLeft") {
      assert(List.productLeft(List(1.0, 2.0, 3.0)) == 6.0)
    }

    it("reverse") {
      assert(List.reverse(List(1, 2, 3, 4)) == List(4, 3, 2, 1))
    }

    it("foldRightUsingFoldLeft") {
      assert(List.foldRightUsingFoldLeft(List("a", "b", "c"), "")(_ + _) == "abc")
    }

    it("append") {
      assert(List.append2(List(1, 2, 3), List(4, 5)) == List(1, 2, 3, 4, 5))
    }

    it("flat") {
      assert(List.flat(List(List(1, 2, 3), List(5, 6, 7), List(4, 7, 9))) == List(1, 2, 3, 5, 6, 7, 4, 7, 9))
    }

    it("flat2") {
      assert(List.flat2(List(List(1, 2, 3), List(5, 6, 7), List(4, 7, 9))) == List(1, 2, 3, 5, 6, 7, 4, 7, 9))
    }

    it("add1") {
      assert(List.add1(List(1, 2, 3)) == List(2, 3, 4))
    }

    it("doubleToStringList") {
      assert(List.doubleToStringList(List(1.2, 4.3)) == List("1.2", "4.3"))
    }

    it("map") {
      assert(List.map(List(1, 2, 3))(_ * 2) == List(2, 4, 6))
    }

    it("filter") {
      assert(List.removeOdds(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)) == List(2, 4, 6, 8, 0))
    }

    it("flatMap") {
      assert(List.flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
    }

    it("filterUsingFlatMap") {
      assert(List.filterUsingFlatMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0))(_ % 2 == 0) == List(2, 4, 6, 8, 0))
    }

    it("addPointwise") {
      assert(List.addPointwise(List(1, 2, 3), List(5, 6, 7, 8)) == List(6, 8, 10))
    }

    it("zipWith") {
      assert(List.zipWith(List(1, 2, 3), List(1, 4, 2, 4, 1))((a, b) => "%d a".format(a + b))
        == List("2 a", "6 a", "5 a"))
    }

    it("hasSubsequence") {
      assert(List.hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3, 4)) == true)
      assert(List.hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3)) == true)
      assert(List.hasSubsequence(List(1, 2, 3, 4, 5, 6), List(1, 2, 3, 4, 5, 6)) == true)
      assert(List.hasSubsequence(List(1, 2, 3, 4, 5, 6), List(3, 2)) == false)
    }
  }

  describe("Tree") {
    it("tree size") {
      assert(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(4))) == 5)
    }

    it("maximum") {
      assert(Tree.maximum(Branch(Branch(Leaf(5), Leaf(2)), Leaf(7))) == 7)
    }

    it("depth") {
      assert(Tree.depth(Branch(Branch(Leaf(5), Leaf(2)), Leaf(7))) == 3)
    }

    it("tree map") {
      assert(Tree.map(Branch(Branch(Leaf(5), Leaf(2)), Leaf(7)))(_ * 2) ==
        Branch(Branch(Leaf(10), Leaf(4)), Leaf(14)))
    }

    it("size2") {
      assert(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(4))) == 5)
    }

    it("maximum2") {
      assert(Tree.maximum(Branch(Branch(Leaf(5), Leaf(2)), Leaf(7))) == 7)
    }

    it("depth2") {
      assert(Tree.depth(Branch(Branch(Leaf(5), Leaf(2)), Leaf(7))) == 3)
    }

    it("map2") {
      assert(Tree.map(Branch(Branch(Leaf(5), Leaf(2)), Leaf(7)))(_ * 2) ==
        Branch(Branch(Leaf(10), Leaf(4)), Leaf(14)))
    }
  }
}
