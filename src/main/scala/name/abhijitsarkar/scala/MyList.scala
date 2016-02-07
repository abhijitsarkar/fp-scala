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
    if (n <= 0) ml
    else
      ml match {
        case Nil => Nil
        case Cons(h, t) => drop(t, n - 1)
      }
  }

  def dropWhile[A](ml: MyList[A], f: A => Boolean): MyList[A] = {
    ml match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t, f)
    }
  }

  @annotation.tailrec
  def foldLeft[A, B](ml: MyList[A], z: B)(f: (B, A) => B): B =
    ml match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def sumUsingFoldLeft(ints: MyList[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productUsingFoldLeft(ds: MyList[Double]): Double = foldLeft(ds, 1.0d)(_ * _)

  def lengthUsingFoldLeft(ints: MyList[Int]): Int = foldLeft(ints, 0)((acc, _) => acc + 1)

}
