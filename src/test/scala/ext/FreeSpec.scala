package ext

import ext.types.OptionMonad.monadInstance
import org.scalatest.freespec.AnyFreeSpec

class FreeSpec extends AnyFreeSpec {
  "Free" - {
    "extract" - {
      "pure" in {
        val free: Free[Option, Int] = Free.pure(42)
        assert(free.extract === Option(42))
      }

      "pure mapped" - {
        val free: Free[Option, Int] = Free.pure(42)
        assert(free.map(_.toString).extract === Option("42"))
      }

      "lift" in {
        val option = Option(42)
        val free = Free.lift(option)

        assert(free.extract === Option(42))
      }

      "lift mapped" in {
        val free = Free.lift(Option(42)).map(_.toString)

        assert(free.extract === Option("42"))
      }
    }

    "foldMap" - {
      case class Box[A](a: A)

      "to Option" in {
        val box = Box(42)
        val free = for {
          a <- Free.lift(box)
          b <- Free.point("***")
          c <- Free.lift(Box(s"$b$a$b"))
        } yield c

        val nt: NaturalTransformation[Box, Option] = new NaturalTransformation[Box, Option] {
          override def apply[A](fa: Box[A]): Option[A] = Option(fa.a)
        }

        assert(free.foldMap(nt) === Option("***42***"))
      }
    }
  }
}
