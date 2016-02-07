package name.abhijitsarkar.scala

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Abhijit Sarkar
  */
class TreeSpec extends FlatSpec with Matchers {
  "Tree" should "count the number of nodes" in {
    val tree = makeTree(List("a", "b", "c", "d"))

    Tree.size(tree) should be(7)
  }

  it should "return the maximum element" in {
    val tree = makeTree(List(1, 2, 3, 4))

    Tree.max(tree) should be(4)
  }

  it should "return the depth" in {
    val tree = makeTree(List("a", "b", "c", "d"))

    Tree.depth(tree) should be(2)
  }

  it should "return all the leaves" in {
    val tree = makeTree(List("a", "b", "c", "d"))

    Tree.leaves(tree) should contain inOrder("a", "b", "c", "d")
  }

  it should "modify each element in a tree with a given function" in {
    val tree = makeTree(List(1, 2, 3, 4))
    val modifiedTree = Tree.map(tree, (a: Int) => a * 2)

    Tree.leaves(modifiedTree) should contain inOrder(2, 4, 6, 8)
  }

  private def makeTree[A](l: List[A]) = {
    assert(l.size % 2 == 0, "Cannot make a tree from odd number of elements.")

    l.map(Leaf(_)).sliding(2, 2).map(leaves => Branch(leaves.head, leaves.last)).reduce(Branch(_, _))
  }
}
