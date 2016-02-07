package name.abhijitsarkar.scala

import scala.collection.mutable.ListBuffer

/**
  * @author Abhijit Sarkar
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/* The solution from the author is more elegant without needing the intermediate `go` functions. */
object Tree {
  def size[A](t: Tree[A]) = {
    def go(s: Int, t: Tree[A]): Int = t match {
      case Leaf(_) => s + 1
      case Branch(l, r) => go(s, l) + go(s, r) + 1
    }

    go(0, t)
  }

  def max(t: Tree[Int]) = {
    def go(s: Int, t: Tree[Int]): Int = t match {
      case Leaf(v) => v max s
      case Branch(l, r) => go(s, l) max go(s, r)
    }

    go(Int.MinValue, t)
  }

  def depth[A](t: Tree[A]) = {
    def go(s: Int, t: Tree[A]): Int = t match {
      case Leaf(_) => s
      case Branch(l, r) => go(s + 1, l) max go(s + 1, r)
    }

    go(0, t)
  }

  def map[A, B](t: Tree[A], f: A => B) = {
    def go(t: Tree[A]): Tree[B] = t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(go(l), go(r))
    }

    go(t)
  }

  import scala.collection.mutable

  def leaves[A](t: Tree[A]) = {
    type ListBuilder = mutable.Builder[A, ListBuffer[A]]

    def go(buff: ListBuilder, tree: Tree[A]): ListBuilder = tree match {
      case Leaf(v) => buff += v
      case Branch(l, r) => go(buff, l); go(buff, r)
    }

    go(ListBuffer.newBuilder[A], t).result
  }
}
