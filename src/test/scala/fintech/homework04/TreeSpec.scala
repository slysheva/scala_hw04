package fintech.homework04
import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  it should "correctly find tree size" in {
    val tree01: Tree[Int] = {
      new Branch[Int](
        new Leaf[Int](3),
        new Branch[Int](Leaf(1), Leaf(2)))
    }
    val tree02: Tree[Int] = { new Leaf[Int](1) }
    val tree03: Tree[String] = {
      new Branch[String](
        new Branch[String](
          new Branch[String](Leaf("1"), Leaf("2")),
          new Branch[String](Leaf("3"), Leaf("4"))
        ),
        new Branch[String](
          new Branch[String](Leaf("5"), Leaf("6")),
          new Branch[String](Leaf("7"), Leaf("8"))
        )
      )
    }
    Tree.size(tree01) should be (3)
    Tree.size(tree02) should be (1)
    Tree.size(tree03) should be (8)
  }

  it should "correctly find max element in tree with ints" in {
    val tree01: Tree[Int] = {
      new Branch[Int](
        new Leaf[Int](3),
        new Branch[Int](Leaf(1), Leaf(2)))
    }
    val tree02: Tree[Int] = { new Leaf[Int](1) }
    val tree03: Tree[Int] = {
      new Branch[Int](
        new Branch[Int](
          new Branch[Int](Leaf(1), Leaf(2)),
          new Branch[Int](Leaf(3), Leaf(4))
        ),
        new Branch[Int](
          new Branch[Int](Leaf(599), Leaf(6)),
          new Branch[Int](Leaf(7), Leaf(8))
        )
      )
    }
    Tree.max(tree01) should be (3)
    Tree.max(tree02) should be (1)
    Tree.max(tree03) should be (599)
  }

  it should "correctly find tree depth" in {
    val tree01: Tree[Int] = {
      new Branch[Int](
        new Leaf[Int](3),
        new Branch[Int](Leaf(1), Leaf(2)))
    }
    val tree02: Tree[Int] = { new Leaf[Int](1) }
    val tree03: Tree[String] = {
      new Branch[String](
        new Branch[String](
          new Branch[String](Leaf("1"), Leaf("2")),
          new Branch[String](Leaf("3"), Leaf("4"))
        ),
        new Branch[String](
          new Branch[String](Leaf("5"), Leaf("6")),
          new Branch[String](Leaf("7"), Leaf("8"))
        )
      )
    }
    Tree.depth(tree01) should be (2)
    Tree.depth(tree02) should be (0)
    Tree.depth(tree03) should be (3)
  }

  "map" should "works correctly" in {
    val tree01: Tree[Int] = {
      new Branch[Int](
        new Leaf[Int](1),
        new Branch[Int](Leaf(2), Leaf(3)))
    }
    val tree02: Tree[String] = {
      new Branch[String](
        new Leaf[String]("a"),
        new Branch[String](Leaf("aa"), Leaf("aaa")))
    }
    Tree.map(tree02)(s => s.length()) == tree01 should be (true)
  }

}
