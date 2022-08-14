package ext

import ext.In.top
import org.scalatest.freespec.AnyFreeSpec

import scala.concurrent.Future

class InSpec extends AnyFreeSpec {
  def f[R[_], A](ra: Option[A])(implicit optionInR: Option In R): R[A] = optionInR.inject(ra)

  "In" - {
    "top" - {
      "should resolve single Stack" in {
        val input = Option(42)
        val expected: Stack.of1[Option]#R[Int] = Stack.Top(input)

        assert(f(input) == expected)
      }

      "should resolve multiple Stack" in {
        val input = Option(42)
        val expected: Stack.of3[Option, List, Future]#R[Int] = Stack.Top(input)

        assert(f(input) == expected)
      }
    }

    "body" - {
      "should resolve middle of Stack" in {
        type Stack2[A] = Stack.of2[List, Option]#R[A]
        val input = Option(42)
        val expected: Stack2[Int] = Stack.Body(Stack.Top(input))

        assert(f[Stack2, Int](input) == expected)
      }
    }
  }
}
