package name.abhijitsarkar.scala

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.runtime.{universe => ru}

/**
  * @author Abhijit Sarkar
  */
class Ch3Spec extends FlatSpec with Matchers {
  "Method findMatch" should "correctly evaluate a match expression for given array" in {
    val inputAndOutput = Table(
      ("input", "output"),
      (MyList(1, 2, 4), 1),
      (MyList(), 42),
      (MyList(1, 2, 3, 4, 5), 3),
      (MyList(5, 4, 3, 2, 1), 15),
      (MyList(10), 10)
    )

    forAll(inputAndOutput) { (ip, op) =>
      Ch3.findMatch(ip) shouldBe op
    }
  }

  "Method tail" should "drop the first element of a list" in {
    val inputAndOutput = Table(
      ("input", "output"),
      (MyList[Int](), Nil),
      (MyList[Int](1), Nil),
      (MyList[Int](1, 2, 3), MyList[Int](2, 3))
    )

    forAll(inputAndOutput) { (ip, op) =>
      MyList.tail(ip) shouldBe op
    }
  }

  "Method drop" should "drop the first n elements of a list" in {
    val inputAndOutput = Table(
      ("input", "n", "output"),
      (MyList[Int](), 1, Nil),
      (MyList[Int](), 5, Nil),
      (MyList[Int](1), 1, Nil),
      (MyList[Int](1), 5, Nil),
      (MyList[Int](1, 2, 3), 1, MyList[Int](2, 3))
    )

    forAll(inputAndOutput) { (ip, n, op) =>
      MyList.drop(ip, n) shouldBe op
    }
  }
}
