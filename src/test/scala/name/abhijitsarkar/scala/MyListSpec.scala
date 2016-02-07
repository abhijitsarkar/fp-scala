package name.abhijitsarkar.scala

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Abhijit Sarkar
  */
class MyListSpec extends FlatSpec with Matchers {
  "MyList" should "drop the first element of a list" in {
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

  it should "drop the first n elements of a list" in {
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
