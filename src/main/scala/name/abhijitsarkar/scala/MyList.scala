package name.abhijitsarkar.scala

/**
  * @author Abhijit Sarkar
  */

sealed trait MyList[+A]

case object Nil extends MyList[Nothing]

case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](ml: MyList[A]): MyList[A] = ml match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def drop[A](ml: MyList[A], n: Int): MyList[A] = {
    (1 to n).foldLeft(ml) { (acc, i) => if (acc == Nil) acc else tail(acc) }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    ???
  }
}
