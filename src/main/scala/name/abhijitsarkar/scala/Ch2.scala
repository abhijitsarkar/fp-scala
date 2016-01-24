package name.abhijitsarkar.scala

/**
  * @author Abhijit Sarkar
  */
object Ch2 {
  /**
    * Q2.2: Implement `isSorted`, which checks whether an `Array[A]` is sorted according to a given
    * comparison function.
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    val sorted = for (it <- as.sliding(2))
      yield (it.length < 2) || ordered(it(0), it(1))

    sorted.find(!_).isEmpty
  }

  def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    (as.isEmpty) || !as.view.zip(as.tail).exists { case (a, b) => !ordered(a, b) }

  /* No need to use a view as 'sliding' returns an Iterator. If we must use a view,
   * need to use 'as.toIndexedSeq.view.sliding' due to bug https://issues.scala-lang.org/browse/SI-6709.
   */
  def isSorted3[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    (as.size < 2) || as.sliding(2).find(it => !ordered(it(0), it(1))).isEmpty

  /**
    * Q2.3: Let's look at another example, ''currying'', which converts a function `f`of two arguments
    * into a function of one argument that partially applies `f`. Here again there's only one implementation
    * that compiles. Write this implementation.
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  /**
    * Q2.4: Implement `uncurry`, which reverses the transformation of `curry`. Note that since `=>` associates to
    * the right, `A => (B => C)` can be written as `A => B => C`.
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  /**
    * Q2.5: Implement the higher-order function that composes two functions.
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
