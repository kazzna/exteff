package ext

import org.scalatest.freespec.AnyFreeSpec

class StackSpec extends AnyFreeSpec {
  "Stack" - {
    "of1" - {
      "should be extracted top container" in {
        val stack: Stack.of1[Option]#R[Int] = Stack.Top(Option(42))
        val expected = Option(42)

        assert(stack.extract == expected)
      }
    }

    "of2" - {
      "should be restructured top container" in {
        val stack2: Stack.of2[Option, List]#R[Int] = Stack.Top(Option(42))
        val expected: Stack.of1[List]#R[Int] = Stack.Top(List(42))

        val nt = new NaturalTransformation[Option, List] {
          override def apply[A](fa: Option[A]): List[A] = fa.toList
        }

        assert(stack2.restruct(nt) == expected)
      }
    }
  }
}
