package ext

import ext.Eff.{Single, Stacked}
import ext.types.OptionMonad.monadInstance
import org.scalatest.freespec.AnyFreeSpec

class EffSpec extends AnyFreeSpec {
  "Eff" - {
    "inject" - {
      "should compose containers" in {
        type R[A] = Stack.of2[Int => *, Option]#R[A]

        val eff1: Eff[R, Int] = Eff.inject(Option(21))
        val eff2: Eff[R, Int] = Eff.inject((i: Int) => i)

        val eff3 = for {
          a <- eff1
          b <- eff2
        } yield a + b

        val func1ToOption = new NaturalTransformation[Int => *, Option] {
          override def apply[B](fa: Int => B): Option[B] = Option(fa(21))
        }

        val eff4 = eff3.restruct(func1ToOption)
        assert(eff4.extract === Option(42))
      }
    }
  }
}
