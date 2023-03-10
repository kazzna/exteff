package ext

import org.scalatest.freespec.AnyFreeSpec
import types.Monad

class FreeMonadSpec extends AnyFreeSpec {
  "Free Monad" - {
    "map3" - {
      def runMap3[F[_]](f1: F[Int], f2: F[Int], f3: F[Int])(implicit F: Monad[F]): F[Int] =
        F.map3(f1)(f2)(f3)((a, b, c) => a + b + c)

      "returns same result with Base Monad" in {
        val e1: Either[List[String], Int] = Right(1)
        val e2: Either[List[String], Int] = Right(2)
        val e3: Either[List[String], Int] = Right(3)

        val f1: Free[Either[List[String], *], Int] = Free.lift(e1)
        val f2: Free[Either[List[String], *], Int] = Free.lift(e2)
        val f3: Free[Either[List[String], *], Int] = Free.lift(e3)

        import types.Either.monadWithLeftSemigroup
        import types.List.semigroup
        val expected = runMap3(e1, e2, e3)
        val actual = runMap3(f1, f2, f3)

        assert(actual.extract === expected)
      }

      "returns same error with Base Monad" in {
        val e1: Either[List[String], Int] = Left(List("1st error"))
        val e2: Either[List[String], Int] = Right(2)
        val e3: Either[List[String], Int] = Left(List("3rd error"))

        val f1: Free[Either[List[String], *], Int] = Free.lift(e1)
        val f2: Free[Either[List[String], *], Int] = Free.lift(e2)
        val f3: Free[Either[List[String], *], Int] = Free.lift(e3)

        import types.Either.monadWithLeftSemigroup
        import types.List.semigroup
        val expected = runMap3(e1, e2, e3)
        val actual = runMap3(f1, f2, f3)

        assert(actual.extract === expected)
      }
    }

    "map4" - {
      def runMap4[F[_]](f1: F[Int], f2: F[Int], f3: F[Int], f4: F[Int])(implicit F: Monad[F]): F[Int] =
        F.map4(f1)(f2)(f3)(f4)((a, b, c, d) => a + b + c + d)

      "returns same result with Base Monad" in {
        val e1: Either[List[String], Int] = Right(1)
        val e2: Either[List[String], Int] = Right(2)
        val e3: Either[List[String], Int] = Right(3)
        val e4: Either[List[String], Int] = Right(4)

        val f1: Free[Either[List[String], *], Int] = Free.lift(e1)
        val f2: Free[Either[List[String], *], Int] = Free.lift(e2)
        val f3: Free[Either[List[String], *], Int] = Free.lift(e3)
        val f4: Free[Either[List[String], *], Int] = Free.lift(e4)

        import types.Either.monadWithLeftSemigroup
        import types.List.semigroup
        val expected = runMap4(e1, e2, e3, e4)
        val actual = runMap4(f1, f2, f3, f4)

        assert(actual.extract === expected)
      }

      "returns same error with Base Monad" in {
        val e1: Either[List[String], Int] = Left(List("1st error"))
        val e2: Either[List[String], Int] = Right(2)
        val e3: Either[List[String], Int] = Left(List("3rd error"))
        val e4: Either[List[String], Int] = Left(List("4th error"))

        val f1: Free[Either[List[String], *], Int] = Free.lift(e1)
        val f2: Free[Either[List[String], *], Int] = Free.lift(e2)
        val f3: Free[Either[List[String], *], Int] = Free.lift(e3)
        val f4: Free[Either[List[String], *], Int] = Free.lift(e4)

        import types.Either.monadWithLeftSemigroup
        import types.List.semigroup
        val expected = runMap4(e1, e2, e3, e4)
        val actual = runMap4(f1, f2, f3, f4)

        assert(actual.extract === expected)
      }
    }

    "map5" - {
      def runMap5[F[_]](f1: F[Int], f2: F[Int], f3: F[Int], f4: F[Int], f5: F[Int])(implicit F: Monad[F]): F[Int] =
        F.map5(f1)(f2)(f3)(f4)(f5)((a, b, c, d, e) => a + b + c + d + e)

      "returns same result with Base Monad" in {
        val e1: Either[List[String], Int] = Right(1)
        val e2: Either[List[String], Int] = Right(2)
        val e3: Either[List[String], Int] = Right(3)
        val e4: Either[List[String], Int] = Right(4)
        val e5: Either[List[String], Int] = Right(5)

        val f1: Free[Either[List[String], *], Int] = Free.lift(e1)
        val f2: Free[Either[List[String], *], Int] = Free.lift(e2)
        val f3: Free[Either[List[String], *], Int] = Free.lift(e3)
        val f4: Free[Either[List[String], *], Int] = Free.lift(e4)
        val f5: Free[Either[List[String], *], Int] = Free.lift(e5)

        import types.Either.monadWithLeftSemigroup
        import types.List.semigroup
        val expected = runMap5(e1, e2, e3, e4, e5)
        val actual = runMap5(f1, f2, f3, f4, f5)

        assert(actual.extract === expected)
      }

      "returns same error with Base Monad" in {
        val e1: Either[List[String], Int] = Left(List("1st error"))
        val e2: Either[List[String], Int] = Right(2)
        val e3: Either[List[String], Int] = Left(List("3rd error"))
        val e4: Either[List[String], Int] = Right(4)
        val e5: Either[List[String], Int] = Left(List("5th error"))

        val f1: Free[Either[List[String], *], Int] = Free.lift(e1)
        val f2: Free[Either[List[String], *], Int] = Free.lift(e2)
        val f3: Free[Either[List[String], *], Int] = Free.lift(e3)
        val f4: Free[Either[List[String], *], Int] = Free.lift(e4)
        val f5: Free[Either[List[String], *], Int] = Free.lift(e5)

        import types.Either.monadWithLeftSemigroup
        import types.List.semigroup
        val expected = runMap5(e1, e2, e3, e4, e5)
        val actual = runMap5(f1, f2, f3, f4, f5)

        assert(actual.extract === expected)
      }
    }

    "map6" - {
      def runMap6[F[_]](
          f1: F[Int],
          f2: F[Int],
          f3: F[Int],
          f4: F[Int],
          f5: F[Int],
          f6: F[Int]
      )(implicit F: Monad[F]): F[Int] =
        F.map6(f1)(f2)(f3)(f4)(f5)(f6)((a, b, c, d, e, f) => a + b + c + d + e + f)

      "returns same result with Base Monad" in {
        val e1: Either[List[String], Int] = Right(1)
        val e2: Either[List[String], Int] = Right(2)
        val e3: Either[List[String], Int] = Right(3)
        val e4: Either[List[String], Int] = Right(4)
        val e5: Either[List[String], Int] = Right(5)
        val e6: Either[List[String], Int] = Right(6)

        val f1: Free[Either[List[String], *], Int] = Free.lift(e1)
        val f2: Free[Either[List[String], *], Int] = Free.lift(e2)
        val f3: Free[Either[List[String], *], Int] = Free.lift(e3)
        val f4: Free[Either[List[String], *], Int] = Free.lift(e4)
        val f5: Free[Either[List[String], *], Int] = Free.lift(e5)
        val f6: Free[Either[List[String], *], Int] = Free.lift(e6)

        import types.Either.monadWithLeftSemigroup
        import types.List.semigroup
        val expected = runMap6(e1, e2, e3, e4, e5, e6)
        val actual = runMap6(f1, f2, f3, f4, f5, f6)

        assert(actual.extract === expected)
      }

      "returns same error with Base Monad" in {
        val e1: Either[List[String], Int] = Left(List("1st error"))
        val e2: Either[List[String], Int] = Right(2)
        val e3: Either[List[String], Int] = Left(List("3rd error"))
        val e4: Either[List[String], Int] = Right(4)
        val e5: Either[List[String], Int] = Left(List("5th error"))
        val e6: Either[List[String], Int] = Left(List("6th error"))

        val f1: Free[Either[List[String], *], Int] = Free.lift(e1)
        val f2: Free[Either[List[String], *], Int] = Free.lift(e2)
        val f3: Free[Either[List[String], *], Int] = Free.lift(e3)
        val f4: Free[Either[List[String], *], Int] = Free.lift(e4)
        val f5: Free[Either[List[String], *], Int] = Free.lift(e5)
        val f6: Free[Either[List[String], *], Int] = Free.lift(e6)

        import types.Either.monadWithLeftSemigroup
        import types.List.semigroup
        val expected = runMap6(e1, e2, e3, e4, e5, e6)
        val actual = runMap6(f1, f2, f3, f4, f5, f6)

        assert(actual.extract === expected)
      }
    }
  }
}
