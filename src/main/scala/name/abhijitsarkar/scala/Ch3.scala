package name.abhijitsarkar.scala

/**
  * @author Abhijit Sarkar
  */
object Ch3 {

  import MyList.sum

  def findMatch(ml: MyList[Int]) = {
    ml match {
      case Cons(x, Cons(2, Cons(4, _))) => println(s"Case 1; List: $ml; Match result: $x."); x
      case Nil => println(s"Case 2; List: $ml; Match result: 42."); 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => val r = x + y; println(s"Case 3; List: $ml; Match result: $r."); r
      case Cons(h, t) => val r = h + sum(t); println(s"Case 4; List: $ml; Match result: $r."); r
      /* This'll never be invoked. A single element list matches the case above, where t is Nil. */
      case _ => println(s"Case 5; List: $ml; Match result: 101."); 101
    }
  }
}


