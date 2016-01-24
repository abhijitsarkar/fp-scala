package name.abhijitsarkar.scala

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.runtime.{universe => ru}

/**
  * @author Abhijit Sarkar
  */
class Ch2Spec extends FlatSpec with Matchers {
  "Method isSorted" should "correctly evaluate whether an array is sorted or not" in {
    val methods = Table(
      ("method"),
      ("isSorted"),
      ("isSorted2"),
      ("isSorted3")
    )
    val inputAndOutput = Table(
      ("input", "output"),
      (Array(1, 2, 3, 4), true),
      (Array(4, 6, 3), false),
      (Array(1), true)
    )

    val im = instanceMirror

    /* The compiler cannot figure out Ordering[Int].lt or Ordering[Int].lt(_, _) in a reflective method invocation.
     * See http://stackoverflow.com/questions/34960244/scala-orderingint-lt-invocation-fails-for-reflective-method-call.
     */
    val lt = Ordering[Int].lt(_, _)

    forAll(methods) { m =>
      val isSortedMethod = ru.typeOf[Ch2.type].decl(ru.TermName(m)).asMethod
      val isSorted = im.reflectMethod(isSortedMethod)

      forAll(inputAndOutput) { (ip, op) =>
        isSorted(ip, lt) shouldBe op
      }
    }
  }

  private def instanceMirror = {
    val m = ru.runtimeMirror(getClass.getClassLoader)
    val mod = ru.typeOf[Ch2.type].termSymbol.asModule
    val mm = m.reflectModule(mod)
    val obj = mm.instance

    m.reflect(obj)
  }
}
