package ext

import ext.transform.BoxToOption
import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.tagobjects.Slow
import test.Box

class FreeSpec extends AnyFreeSpec {
  def assertPure[F[_], A](free: Free[F, A])(value: A): Assertion = free match {
    case Free.Pure(a) => assert(a === value)
    case free => fail(s"Free.Pure expected but $free found.")
  }

  def assertLifted[F[_], A](free: Free[F, A])(
      assert: F[A] => Assertion
  ): Assertion = free match {
    case Free.Lifted(fa) => assert(fa)
    case free => fail(s"Free.Lifted expected but $free found.")
  }

  def assertImpure[F[_], I, A](free: Free[F, A])(
      assert: (F[I], Free.Arrows[F, I, A]) => Assertion
  ): Assertion = free match {
    case impure @ Free.Impure(_, _) =>
      assert(impure.fi.asInstanceOf[F[I]], impure.arrows.asInstanceOf[Free.Arrows[F, I, A]])
    case free => fail(s"Free.Impure expected but $free found.")
  }

  "Free object" - {
    "pure" - {
      "creates Free.Pure" in {
        assertPure(Free.pure[Box, Int](42))(42)
      }
    }

    "point" - {
      "creates Free.Pure" in {
        assertPure(Free.point[Option, Int](42))(42)
      }
    }

    "lift" - {
      "creates Free.Impure" in {
        assertLifted[Box, Int](Free.lift(Box(42))) { fi =>
          assert(fi === Box(42))
        }
      }
    }
  }

  "Free instance" - {
    "Pure" - {
      "map" - {
        "returns Free.Pure" in {
          val pure = Free.pure[Box, Int](41)
          assertPure(pure.map(_ + 1))(42)
        }
      }

      "ap" - {
        "with Free.Pure" - {
          "returns Free.Pure" in {
            val i = 41
            val f = (i: Int) => i + 1
            val pure1 = Free.pure[Box, Int](i)
            val pure2 = Free.pure[Box, Int => Int](f)

            val actual = pure1.ap(pure2)
            assertPure(actual)(42)
          }
        }

        "with Free.Lifted" - {
          "returns Free.Impure" in {
            val a = 41
            val pure = Free.pure[Box, Int](a)
            val f = (i: Int) => i + 1
            val bf = Box(f)
            val lifted = Free.lift(bf)

            val actual = pure.ap(lifted)
            assertImpure[Box, Int => Int, Int](actual) { (fi, arrows) =>
              assert(fi === bf)
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }

          "keeps error" in {
            val pure = Free.pure[Either[List[String], *], Int](42)
            val error = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Int => String](Left(error))

            val actual = pure.ap(lifted)
            assertImpure[Either[List[String], *], Int => String, String](actual) { (fi, arrows) =>
              assert(fi === Left(error))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error))
            }
          }
        }

        "with Free.Impure" - {
          "returns Free.Impure" in {
            val pure = Free.pure[Box, Int](41)
            val f = (s: String) => Free.lift(Box((i: Int) => i + s.length))
            val ba = Box("a")
            val impure = Free.lift(ba).flatMap(f)

            val actual = pure.ap(impure)
            assertImpure[Box, String, Int](actual) { (fi, arrows) =>
              assert(fi === ba)
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }

          "returns error in flatMap" in {
            val pure = Free.pure[Either[List[String], *], Int](42)
            val error = "Operation failed."
            val f = (s: String) => Free.lift[Either[List[String], *], Int => String](Left(List(s)))
            val impure = Free.lift[Either[List[String], *], String](Right(error)).flatMap(f)

            val actual = pure.ap(impure)
            assertImpure[Either[List[String], *], Int => String, String](actual) { (fi, arrows) =>
              assert(fi === Right(error))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(List(error)))
            }
          }
        }
      }

      "map2" - {
        "with Free.Pure" - {
          "returns Free.Pure" in {
            val pure1 = Free.pure[Box, Int](20)
            val pure2 = Free.pure[Box, Int](22)

            val actual = pure1.map2(pure2)((a, b) => a + b)
            assertPure(actual)(42)
          }
        }

        "with Free.Lifted" - {
          "returns Free.Impure" in {
            val pure = Free.pure[Box, Int](20)
            val lifted = Free.lift(Box(22))

            val actual = pure.map2(lifted)((a, b) => a + b)
            assertImpure[Box, Int, Int](actual) { (fi, arrows) =>
              assert(fi === Box(22))
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }

          "returns error in Free.Impure" in {
            val pure = Free.pure[Either[List[String], *], Int](42)
            val error = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Double](Left(error))

            val actual = pure.map2(lifted)((i, d) => (i.toDouble + d).toString)
            assertImpure[Either[List[String], *], Double, String](actual) { (fi, arrows) =>
              assert(fi === Left(error))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error))
            }
          }
        }

        "with Free.Impure" - {
          "returns Free.Impure" in {
            val pure = Free.pure[Box, Int](20)
            val impure = Free.lift(Box(21)).flatMap(i => Free.lift(Box(i + 1)))

            val actual = pure.map2(impure)((a, b) => a + b)
            assertImpure[Box, Int, Int](actual) { (fi, arrows) =>
              assert(fi === Box(21))
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }

          "returns error in Free.Impure" in {
            val pure = Free.pure[Either[List[String], *], Int](42)
            val error = "Operation failed."
            val f = (s: String) => Free.lift[Either[List[String], *], Double](Left(List(s)))
            val impure = Free.lift[Either[List[String], *], String](Right(error)).flatMap(f)

            val actual = pure.map2(impure)((i, d) => (i.toDouble + d).toString)
            assertImpure[Either[List[String], *], Double, String](actual) { (fi, arrows) =>
              assert(fi === Right(error))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(List(error)))
            }
          }
        }
      }

      "flatMap" - {
        "to Free.Pure" - {
          "returns Free.Pure" in {
            val i = 41
            val pure = Free.pure[Box, Int](i)
            val f = (i: Int) => Free.pure[Box, Int](i + 1)

            val actual = pure.flatMap(f)
            assertPure(actual)(42)
            assert(actual === f(i))
          }
        }

        "to Free.Lifted" - {
          "returns Free.Impure" in {
            val i = 41
            val pure = Free.pure[Box, Int](41)
            val f = (i: Int) => Free.lift(Box(i + 1))

            val actual = pure.flatMap(f)
            assertLifted[Box, Int](actual) { _ =>
              assert(actual === f(i))
            }
          }
        }

        "to Free.Impure" - {
          "returns Free.Impure" in {
            val i = 41
            val pure = Free.pure[Box, Int](41)
            val f = (i: Int) => Free.lift(Box(i)).map(_ + 1)

            val actual = pure.flatMap(f)
            assertImpure[Box, Int => Int, Int](actual) { (_, _) =>
              assert(actual === f(i))
            }
          }
        }
      }

      "transform" - {
        "returns new Pure" in {
          val pure = Free.pure[Box, Int](42)

          val actual = pure.transform(BoxToOption)
          assertPure(actual)(42)

          import types.Option.monad
          assert(actual.extract === Option(42))
        }
      }

      "extract" - {
        "returns inner value" in {
          import types.Option.monad
          val pure = Free.pure[Option, Int](42)

          val actual = pure.extract
          assert(actual === Option(42))
        }
      }

      "foldMap" - {
        "returns extracted instance" in {
          import types.Option.monad

          val pure = Free.pure[Box, Int](42)
          assert(pure.foldMap(BoxToOption) === Option(42))
        }
      }

      "flatten" - {
        "contains Free.Pure" - {
          "returns Free.Pure" in {
            val pure2 = Free.pure[Box, Int](42)
            val pure1 = Free.pure[Box, Free[Box, Int]](pure2)

            assert(pure1.flatten === pure2)
          }
        }

        "contains Free.Lifted" - {
          "returns Free.Lifted" in {
            val lifted = Free.lift(Box(42))
            val pure = Free.pure[Box, Free[Box, Int]](lifted)

            assert(pure.flatten === lifted)
          }
        }

        "contains Free.Impure" - {
          "returns Free.Impure" in {
            val impure = Free.lift(Box(41)).flatMap(i => Free.lift(Box(i + 1)))
            val pure = Free.pure[Box, Free[Box, Int]](impure)

            assert(pure.flatten === impure)
          }
        }
      }

      "apply" - {
        "Free.Pure" - {
          "returns Free.Pure" in {
            val pure1 = Free.pure[Box, Int => Double](_.toDouble)
            val pure2 = Free.pure[Box, Int](42)
            assert(pure1(pure2) === Free.pure[Box, Double](42d))
          }
        }

        "Free.Lifted" - {
          "returns Free.Impure" in {
            val f = (_: Int).toDouble
            val pure = Free.pure[Box, Int => Double](f)
            val lifted = Free.lift(Box(42))
            val expected = lifted.map(f)
            assert(pure(lifted) === expected)
          }

          "returns error in Free.Impure" in {
            val f = (_: Int).toDouble
            val pure = Free.pure[Either[String, *], Int => Double](f)
            val lifted = Free.lift[Either[String, *], Int](Left("Error occurred."))
            val expected = lifted.map(f)
            assert(pure(lifted) === expected)
          }
        }

        "Free.Impure" - {
          "returns Free.Impure" in {
            val f = (_: Int).toDouble
            val pure = Free.pure[Box, Int => Double](f)
            val impure = Free.lift(Box("abc")).map(_.length)
            val expected = impure.map(f)
            assert(pure(impure) === expected)
          }

          "returns error in Free.Impure" in {
            val f = (_: Int).toDouble
            val pure = Free.pure[Either[String, *], Int => Double](f)
            val impure = Free.lift[Either[String, *], String](Left("Error occurred.")).map(_.length)
            val expected = impure.map(f)
            assert(pure(impure) === expected)
          }
        }
      }
    }

    "Lifted" - {
      "map" - {
        "returns Free.Impure" in {
          val lifted = Free.lift(Box(41))
          val f = (i: Int) => i + 1

          val actual = lifted.map(f)
          assertImpure[Box, Int, Int](actual) { (fi, arrows) =>
            assert(fi == Box(41))
            assert(arrows === Free.Arrows.map(f))
          }
        }

        "keeps error" in {
          val error = List("Operation failed.")
          val ea: Either[List[String], Int] = Left(error)
          val lifted = Free.lift(ea)
          val f: Int => Int = _ + 1

          val actual = lifted.map(f)
          assertImpure[Either[List[String], *], Int, Int](actual) { (fi, arrows) =>
            assert(fi === ea)
            assert(arrows === Free.Arrows.map(f))
          }
        }
      }

      "ap" - {
        "with Free.Pure" - {
          "returns Free.Impure" in {
            val bi = Box(42)
            val lifted = Free.lift(bi)
            val f = (i: Int) => i.toDouble
            val pure = Free.pure[Box, Int => Double](f)

            val actual = lifted.ap(pure)
            assertImpure[Box, Int, Double](actual) { (fi, arrows) =>
              assert(fi === bi)
              assert(arrows === Free.Arrows.apply(pure))
            }
          }

          "keeps error" in {
            val error = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error))
            val pure = Free.pure[Either[List[String], *], Int => Double](i => i.toDouble)

            val actual = lifted.ap(pure)
            assertImpure[Either[List[String], *], Int, Double](actual) { (fi, arrows) =>
              assert(fi === Left(error))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error))
            }
          }
        }

        "with Free.Lifted" - {
          "returns Free.Impure" in {
            val ba = Box(42)
            val lifted1 = Free.lift(ba)
            val f = (i: Int) => i.toDouble
            val bf = Box(f)
            val lifted2 = Free.lift(bf)

            val actual = lifted1.ap(lifted2)
            assertImpure[Box, Int, Double](actual) { (fi, arrows) =>
              assert(fi === ba)
              assert(arrows === Free.Arrows.lift(bf))
            }
          }

          "merges errors" in {
            val error1 = List("Operation failed.")
            val lifted1 = Free.lift[Either[List[String], *], Int](Left(error1))
            val error2 = List("The other operation is also failed.")
            val lifted2 = Free.lift[Either[List[String], *], Int => Double](Left(error2))

            val actual = lifted1.ap(lifted2)
            assertImpure[Either[List[String], *], Int, Double](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2))
            }
          }
        }

        "with Free.Impure" - {
          "returns Free.Impure" in {
            val box = Box(41)
            val lifted = Free.lift(box)
            val f = (s: String) => Free.lift(Box((i: Int) => i + s.length))
            val ba = Box("a")
            val impure = Free.lift(ba).flatMap(f)

            val actual = lifted.ap(impure)
            assertImpure[Box, Int, Int](actual) { (fi, arrows) =>
              assert(fi === box)
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }

          "keeps errors" in {
            val error1 = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error1))
            val error2 = List("The other operation is also failed.")
            val free = Free.lift[Either[List[String], *], String](Left(error2))
            val impure = free.map(s => (i: Int) => (s.length + i).toDouble)

            val actual = lifted.ap(impure)
            assertImpure[Either[List[String], *], Int, Double](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2))
            }
          }

          "flatMap discards trailing errors" in {
            val error1 = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error1))
            val error2 = "The other operation is also failed."
            val f = (s: String) => Free.lift[Either[List[String], *], Int => Double](Left(List(s)))
            val impure = Free.lift[Either[List[String], *], String](Right(error2)).flatMap(f)

            val actual = lifted.ap(impure)
            assertImpure[Either[List[String], *], Int, Double](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1))
            }
          }
        }
      }

      "map2" - {
        "with Free.Pure" - {
          "returns Free.Impure" in {
            val lifted = Free.lift(Box(20))
            val pure = Free.pure[Box, Int](22)

            val actual = lifted.map2(pure)((a, b) => a + b)
            assertImpure[Box, Int, Int](actual) { (fi, arrows) =>
              assert(fi === Box(20))
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }

          "keeps error" in {
            val error = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error))
            val pure = Free.pure[Either[List[String], *], Int](22)

            val actual = lifted.map2(pure)((a, b) => a + b)
            assertImpure[Either[List[String], *], Int, Int](actual) { (fi, arrows) =>
              assert(fi === Left(error))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error))
            }
          }
        }

        "with Free.Lifted" - {
          "returns Free.Impure" in {
            val lifted1 = Free.lift(Box(20))
            val lifted2 = Free.lift(Box(22))

            val actual = lifted1.map2(lifted2)((a, b) => a + b)
            assertImpure[Box, Int, Int](actual) { (fi, arrows) =>
              assert(fi === Box(20))
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }

          "merges errors" in {
            val error1 = List("Operation failed.")
            val lifted1 = Free.lift[Either[List[String], *], Int](Left(error1))
            val error2 = List("The other operation is also failed.")
            val lifted2 = Free.lift[Either[List[String], *], Int](Left(error2))

            val actual = lifted1.map2(lifted2)((a, b) => a + b)
            assertImpure[Either[List[String], *], Int, Int](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2))
            }
          }
        }

        "with Free.Impure" - {
          "returns Free.Impure" in {
            val lifted = Free.lift(Box(20))
            val impure = Free.lift(Box(21)).flatMap(i => Free.lift(Box(i + 1)))

            val actual = lifted.map2(impure)((a, b) => a + b)
            assertImpure[Box, Int, Int](actual) { (fi, arrows) =>
              assert(fi === Box(20))
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }

          "merges errors" in {
            val error1 = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error1))
            val error2 = List("The other operation is also failed.")
            val free = Free.lift[Either[List[String], *], Int](Left(error2))
            val error3 = List("Another operation is also failed.")
            val impure = free.ap(Free.lift[Either[List[String], *], Int => Int](Left(error3)))

            val actual = lifted.map2(impure)((a, b) => a + b)
            assertImpure[Either[List[String], *], Int, Int](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2 ++ error3))
            }
          }

          "keeps errors before flatMap" in {
            val error1 = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error1))
            val error2 = List("The other operation is also failed.")
            val free = Free.lift[Either[List[String], *], Int](Left(error2))
            val error3 = List("Another operation is also failed.")
            val impure = free.flatMap(_ => Free.lift[Either[List[String], *], Int](Left(error3)))

            val actual = lifted.map2(impure)((a, b) => a + b)
            assertImpure[Either[List[String], *], Int, Int](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2))
            }
          }
        }
      }

      "flatMap" - {
        "to Free.Pure" - {
          "returns Free.Impure" in {
            val box = Box(41)
            val lifted = Free.lift(box)
            val f = (i: Int) => Free.pure[Box, String]((i + 1).toString)

            val actual = lifted.flatMap(f)
            assertImpure[Box, Int, String](actual) { (fi, arrows) =>
              assert(fi == box)
              import types.Box.monad
              assert(arrows.run(fi) === Box("42"))
            }
          }

          "keeps error before flatMap" in {
            val error1 = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error1))
            val f = (i: Int) => Free.pure[Either[List[String], *], String](i.toString)

            val actual = lifted.flatMap(f)
            assertImpure[Either[List[String], *], Int, String](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1))
            }
          }
        }

        "to Free.Lifted" - {
          "returns Free.Impure" in {
            val box = Box(41)
            val lifted = Free.lift(box)
            val f = (i: Int) => Free.lift(Box(i + 1))

            val actual1 = lifted.flatMap(f)
            assertImpure[Box, Int, Int](actual1) { (fi, arrows) =>
              assert(fi === box)
              assert(arrows === Free.Arrows.bind(f))
            }

            val g = (i: Int) => Free.lift(Box(i.toDouble))
            val actual2 = actual1.flatMap(g)
            assertImpure[Box, Int, Double](actual2) { (fi, arrows) =>
              assert(fi === box)
              assert(arrows == Free.Arrows.bind(f).compose(Free.Arrows.bind(g)))
            }
          }

          "keeps error before flatMap" in {
            val error1 = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error1))
            val error2 = List("The other operation is also failed.")
            val f = (_: Int) => Free.lift[Either[List[String], *], String](Left(error2))

            val actual = lifted.flatMap(f)
            assertImpure[Either[List[String], *], Int, String](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1))
            }
          }
        }

        "to Free.Impure" - {
          "returns Free.Impure" in {
            val box = Box(41)
            val lifted = Free.lift(box)
            val f = (i: Int) => Free.lift(Box(i)).map(_ + 1)

            val actual1 = lifted.flatMap(f)
            assertImpure[Box, Int, Int](actual1) { (fi, arrows) =>
              assert(fi === box)
              assert(arrows === Free.Arrows.bind(f))
            }

            val g = (i: Int) => Free.lift(Box(i.toDouble))
            val actual2 = actual1.flatMap(g)
            assertImpure[Box, Int, Double](actual2) { (fi, arrows) =>
              assert(fi === box)
              assert(arrows == Free.Arrows.bind(f).compose(Free.Arrows.bind(g)))
            }
          }

          "keeps error before flatMap" in {
            val error1 = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error1))
            val error2 = List("The other operation is also failed.")
            val f = (_: Int) => Free.lift[Either[List[String], *], Int](Left(error2)).map(_.toString)

            val actual = lifted.flatMap(f)
            assertImpure[Either[List[String], *], Int, String](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1))
            }
          }
        }
      }

      "transform" - {
        "returns transformed Free" in {
          val lifted = Free.lift(Box(42))

          val actual = lifted.transform(BoxToOption)
          assertLifted[Option, Int](actual) { fi =>
            assert(fi === Option(42))
          }
        }
      }

      "extract" - {
        "returns lifted value" in {
          val option = Option(42)
          val lifted = Free.lift(option)

          import types.Option.monad
          val actual = lifted.extract
          assert(actual === option)
        }
      }

      "foldMap" - {
        "returns extracted instance" in {
          import types.Option.monad
          val lifted = Free.lift(Box(42))
          assert(lifted.foldMap(BoxToOption) === Option(42))
        }
      }

      "flatten" - {
        "contains Free.Pure" - {
          "returns Free.Impure" in {
            val pure = Free.pure[Box, Int](42)
            val box = Box(pure)
            val lifted = Free.lift(box)

            val actual = lifted.flatten
            assertImpure[Box, Free[Box, Int], Int](actual) { (fi, arrows) =>
              assert(fi === box)
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }
        }

        "contains Free.Lifted" - {
          "returns Free.Impure" in {
            val lifted2 = Free.lift(Box(42))
            val box = Box(lifted2)
            val lifted1 = Free.lift(box)

            val actual = lifted1.flatten
            assertImpure[Box, Free[Box, Int], Int](actual) { (fi, arrows) =>
              assert(fi === box)
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }
        }

        "contains Free.Impure" - {
          "returns Free.Impure" in {
            val ba = Box(41)
            val impure = Free.lift(ba).flatMap(i => Free.lift(Box(i + 1)))
            val box = Box(impure)
            val lifted = Free.lift(box)

            val actual = lifted.flatten
            assertImpure[Box, Free[Box, Int], Int](actual) { (fi, arrows) =>
              assert(fi === box)
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }
        }
      }

      "apply" - {
        "Free.Pure" - {
          "returns Free.Impure" in {
            val f = (_: Int).toDouble
            val lifted = Free.lift(Box(f))
            val pure = Free.pure[Box, Int](42)
            assertImpure[Box, Int => Double, Double](lifted(pure)) { (fi, arrows) =>
              assert(fi === Box(f))
              import types.Box.monad
              assert(arrows.run(fi) === Box(42d))
            }
          }

          "keeps error on Free.Lifted" in {
            val error: Either[List[String], Int => Double] = Left(List("Error occurred."))
            val lifted = Free.lift(error)
            val pure = Free.pure[Either[List[String], *], Int](42)
            assertImpure[Either[List[String], *], Int => Double, Double](lifted(pure)) { (fi, arrows) =>
              assert(fi === error)
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === error)
            }
          }
        }

        "Free.Lifted" - {
          "returns Free.Impure" in {
            val f = (_: Int).toDouble
            val lifted1 = Free.lift(Box(f))
            val lifted2 = Free.lift(Box(42))
            assertImpure[Box, Int => Double, Double](lifted1(lifted2)) { (fi, arrows) =>
              assert(fi === Box(f))
              import types.Box.monad
              assert(arrows.run(fi) === Box(42d))
            }
          }

          "merges errors in Free" in {
            val error1 = List("First error occurred.")
            val error2 = List("Another error occurred.")
            val either1: Either[List[String], Int => Double] = Left(error1)
            val either2: Either[List[String], Int] = Left(error2)
            val lifted1 = Free.lift(either1)
            val lifted2 = Free.lift(either2)
            assertImpure[Either[List[String], *], Int => Double, Double](lifted1(lifted2)) { (fi, arrows) =>
              assert(fi === either1)
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2))
            }
          }
        }

        "Free.Impure" - {
          "returns Free.Impure" in {
            val f = (_: Int).toDouble
            val lifted = Free.lift(Box(f))
            val impure = Free.lift(Box("abc")).map(_.length)
            assertImpure[Box, Int => Double, Double](lifted(impure)) { (fi, arrows) =>
              assert(fi === Box(f))
              import types.Box.monad
              assert(arrows.run(fi) === Box(3d))
            }
          }

          "merges errors in Free" in {
            val error1 = List("First error occurred.")
            val error2 = List("Another error occurred.")
            val either1: Either[List[String], Int => Double] = Left(error1)
            val either2: Either[List[String], String] = Left(error2)
            val lifted1 = Free.lift(either1)
            val lifted2 = Free.lift(either2).map(_.length)
            assertImpure[Either[List[String], *], Int => Double, Double](lifted1(lifted2)) { (fi, arrows) =>
              assert(fi === either1)
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2))
            }
          }
        }
      }
    }

    "Impure" - {
      "map" - {
        "returns Free.Impure" in {
          val impure = Free.lift(Box(20)).flatMap(i => Free.lift(Box(i * 2)))

          val actual = impure.map(_ + 2)
          assertImpure[Box, Int, Int](actual) { (fi, arrows) =>
            assert(fi == Box(20))
            import types.Box.monad
            assert(arrows.run(fi) === Box(42))
          }
        }

        "keeps error" in {
          val error = List("Operation failed.")
          val impure = Free.lift[Either[List[String], *], Int](Left(error)).map(_ * 2)

          val actual = impure.map(_ + 2)
          assertImpure[Either[List[String], *], Int, Int](actual) { (fi, arrows) =>
            assert(fi == Left(error))
            import types.Either.monadWithLeftSemigroup
            import types.List.semigroup
            assert(arrows.run(fi) === Left(error))
          }
        }
      }

      "ap" - {
        "with Free.Pure" - {
          "returns Free.Impure" in {
            val impure = Free.lift(Box(41)).flatMap(i => Free.lift(Box(i + 1)))
            val f = (i: Int) => i.toDouble
            val pure = Free.pure[Box, Int => Double](f)

            val actual = impure.ap(pure)
            assertImpure[Box, Int, Double](actual) { (fi, arrows) =>
              assert(fi === Box(41))
              import types.Box.monad
              assert(arrows.run(fi) === Box(42d))
            }
          }

          "merges errors" in {
            val error = List("Operation failed.")
            val impure = Free.lift[Either[List[String], *], Int](Left(error)).map(_ * 2)
            val f = (i: Int) => i.toDouble
            val pure = Free.pure[Either[List[String], *], Int => Double](f)

            val actual = impure.ap(pure)
            assertImpure[Either[List[String], *], Int, Double](actual) { (fi, arrows) =>
              assert(fi == Left(error))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error))
            }
          }
        }

        "with Free.Lifted" - {
          "returns Free.Impure" in {
            val impure = Free.lift(Box(41)).flatMap(i => Free.lift(Box(i + 1)))
            val f = (i: Int) => i.toDouble
            val bf = Box(f)
            val lifted = Free.lift(bf)

            val actual = impure.ap(lifted)
            assertImpure[Box, Int, Double](actual) { (fi, arrows) =>
              assert(fi === Box(41))
              import types.Box.monad
              assert(arrows.run(fi) === Box(42d))
            }
          }

          "merges errors" in {
            val error1 = List("Operation failed.")
            val impure = Free.lift[Either[List[String], *], Int](Left(error1)).map(_ * 2)
            val error2 = List("The other operation is also failed.")
            val lifted = Free.lift[Either[List[String], *], Int => Double](Left(error2))

            val actual = impure.ap(lifted)
            assertImpure[Either[List[String], *], Int, Double](actual) { (fi, arrows) =>
              assert(fi == Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2))
            }
          }
        }

        "with Free.Impure" - {
          "returns Free.Impure" in {
            val box = Box(40)
            val impure1 = Free.lift(box).flatMap(i => Free.lift(Box(i + 1)))
            val f = (s: String) => Free.lift(Box((i: Int) => i + s.length))
            val ba = Box("a")
            val impure2 = Free.lift(ba).flatMap(f)

            val actual = impure1.ap(impure2)
            assertImpure[Box, Int, Int](actual) { (fi, arrows) =>
              assert(fi === box)
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }

          "merges errors before flatMap" in {
            val error1 = List("Operation failed.")
            val lifted1 = Free.lift[Either[List[String], *], Int](Left(error1))
            val error2 = List("The other operation is also failed.")
            val lifted2 = Free.lift[Either[List[String], *], Int => Int](Left(error2))
            val impure1 = lifted1.ap(lifted2)
            val error3 = List("Operation failed 3.")
            val lifted3 = Free.lift[Either[List[String], *], String](Left(error3))
            val error4 = List("Operation failed 4.")
            val f = (_: String) => Free.lift[Either[List[String], *], Int => Double](Left(error4))
            val impure2 = lifted3.flatMap(f)

            val actual = impure1.ap(impure2)
            assertImpure[Either[List[String], *], Int, Double](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2 ++ error3))
            }
          }
        }
      }

      "map2" - {
        "with Free.Pure" - {
          "returns Free.Impure" in {
            val impure = Free.lift(Box(10)).flatMap(i => Free.lift(Box(i * 2)))
            val pure = Free.pure[Box, Int](22)

            val actual = impure.map2(pure)((a, b) => a + b)
            assertImpure[Box, Int, Int](actual) { (fi, arrows) =>
              assert(fi === Box(10))
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }

          "keeps errors" in {
            val error1 = List("Operation failed.")
            val lifted1 = Free.lift[Either[List[String], *], Int](Left(error1))
            val error2 = List("The other operation is also failed.")
            val lifted2 = Free.lift[Either[List[String], *], Int => Int](Left(error2))
            val impure = lifted1.ap(lifted2)
            val pure = Free.pure[Either[List[String], *], Int](22)

            val actual = impure.map2(pure)((a, b) => a + b)
            assertImpure[Either[List[String], *], Int, Int](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2))
            }
          }
        }

        "with Free.Lifted" - {
          "returns Free.Impure" in {
            val impure = Free.lift(Box(10)).flatMap(i => Free.lift(Box(i * 2)))
            val lifted = Free.lift(Box(22))

            val actual = impure.map2(lifted)((a, b) => a + b)
            assertImpure[Box, Int, Int](actual) { (fi, arrows) =>
              assert(fi === Box(10))
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }

          "keeps error before flatMap" in {
            val error1 = List("Operation failed.")
            val lifted1 = Free.lift[Either[List[String], *], Int](Left(error1))
            val impure = lifted1.flatMap(i => Free.lift(Right(i * 2)))
            val error2 = List("The other operation is also failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error2))

            val actual = impure.map2(lifted)((a, b) => a + b)
            assertImpure[Either[List[String], *], Int, Int](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1))
            }
          }

          "merges errors" in {
            val error1 = List("Operation failed.")
            val lifted1 = Free.lift[Either[List[String], *], Int](Left(error1))
            val impure = lifted1.ap(Free.lift(Right((i: Int) => i * 2)))
            val error2 = List("The other operation is also failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error2))

            val actual = impure.map2(lifted)((a, b) => a + b)
            assertImpure[Either[List[String], *], Int, Int](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2))
            }
          }

          "merges errors after flatMap" in {
            val lifted1 = Free.lift[Either[List[String], *], Int](Right(10))
            val error1 = List("Operation failed.")
            val impure = lifted1.flatMap(_ => Free.lift(Left[List[String], Int](error1)))
            val error2 = List("The other operation is also failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error2))

            val actual = impure.map2(lifted)((a, b) => a + b)
            assertImpure[Either[List[String], *], Int, Int](actual) { (fi, arrows) =>
              assert(fi === Right(10))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2))
            }
          }
        }

        "with Free.Impure" - {
          "returns Free.Impure" in {
            val impure1 = Free.lift(Box(10)).flatMap(i => Free.lift(Box(i * 2)))
            val impure2 = Free.lift(Box(21)).flatMap(i => Free.lift(Box(i + 1)))

            val actual = impure1.map2(impure2)((a, b) => a + b)
            assertImpure[Box, Int, Int](actual) { (fi, arrows) =>
              assert(fi === Box(10))
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }

          "keeps error before flatMap" in {
            val error1 = List("Operation failed.")
            val lifted1 = Free.lift[Either[List[String], *], Int](Left(error1))
            val impure1 = lifted1.flatMap(i => Free.lift(Right(i * 2)))
            val error2 = List("The other operation is also failed.")
            val lifted2 = Free.lift[Either[List[String], *], Int](Left(error2))
            val impure2 = lifted2.flatMap(i => Free.lift(Right(i + 1)))

            val actual = impure1.map2(impure2)((a, b) => a + b)
            assertImpure[Either[List[String], *], Int, Int](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1))
            }
          }

          "merges errors" in {
            val error1 = List("Operation failed.")
            val lifted1 = Free.lift[Either[List[String], *], Int](Left(error1))
            val impure1 = lifted1.map(i => i * 2)
            val error2 = List("The other operation is also failed.")
            val lifted2 = Free.lift[Either[List[String], *], Int](Left(error2))
            val impure2 = lifted2.map(i => i + 1)

            val actual = impure1.map2(impure2)((a, b) => a + b)
            assertImpure[Either[List[String], *], Int, Int](actual) { (fi, arrows) =>
              assert(fi === Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2))
            }
          }
        }
      }

      "flatMap" - {
        "to Free.Pure" - {
          "returns Free.Impure" in {
            val box = Box(20)
            val impure = Free.lift(box).map(_ * 2)
            val f = (i: Int) => Free.pure[Box, String]((i + 2).toString)

            val actual = impure.flatMap(f)
            assertImpure[Box, Int, String](actual) { (fi, arrows) =>
              assert(fi == box)
              import types.Box.monad
              assert(arrows.run(fi) === Box("42"))
            }
          }

          "keeps errors" in {
            val error = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error))
            val impure = lifted.map(i => i * 2)
            val f = (i: Int) => Free.pure[Either[List[String], *], String]((i + 2).toString)

            val actual = impure.flatMap(f)
            assertImpure[Either[List[String], *], Int, String](actual) { (fi, arrows) =>
              assert(fi == Left(error))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error))
            }
          }
        }

        "to Free.Lifted" - {
          "returns Free.Impure" in {
            val box = Box(20)
            val impure = Free.lift(box).map(_ * 2)
            val function1 = (i: Int) => Free.lift(Box(i + 2))

            val actual1 = impure.flatMap(function1)
            assertImpure[Box, Int, Int](actual1) { (fi, arrows) =>
              assert(fi === box)
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }

            val function2 = (i: Int) => Free.lift(Box(i.toDouble))
            val actual2 = actual1.flatMap(function2)
            assertImpure[Box, Int, Double](actual2) { (fi, arrows) =>
              assert(fi === box)
              import types.Box.monad
              assert(arrows.run(fi) === Box(42d))
            }
          }

          "keeps errors before flatMap" in {
            val error1 = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error1))
            val impure = lifted.map(i => i * 2)
            val error2 = List("The other operation is also failed.")
            val f = (_: Int) => Free.lift[Either[List[String], *], String](Left(error2))

            val actual = impure.flatMap(f)
            assertImpure[Either[List[String], *], Int, String](actual) { (fi, arrows) =>
              assert(fi == Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1))
            }
          }

          "merges errors after flatMap" in {
            val lifted = Free.lift[Either[List[String], *], Int](Right(42))
            val impure = lifted.map(i => i * 2)
            val error1 = List("Operation failed.")
            val f = (_: Int) => Free.lift[Either[List[String], *], String](Left(error1))
            val error2 = List("The other operation is also failed.")
            val g = f.andThen(_.ap(Free.lift[Either[List[String], *], String => String](Left(error2))))

            val actual = impure.flatMap(g)
            assertImpure[Either[List[String], *], Int, String](actual) { (fi, arrows) =>
              assert(fi == Right(42))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2))
            }
          }
        }

        "to Free.Impure" - {
          "returns Free.Impure" in {
            val box = Box(20)
            val impure = Free.lift(box).map(_ * 2)
            val function1 = (i: Int) => Free.lift(Box(i)).map(_ + 2)

            val actual1 = impure.flatMap(function1)
            assertImpure[Box, Int, Int](actual1) { (fi, arrows) =>
              assert(fi === box)
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }

            val function2 = (i: Int) => Free.lift(Box(i)).map(_.toDouble)
            val actual2 = actual1.flatMap(function2)
            assertImpure[Box, Int, Double](actual2) { (fi, arrows) =>
              assert(fi === box)
              import types.Box.monad
              assert(arrows.run(fi) === Box(42d))
            }
          }

          "keeps errors before flatMap" in {
            val error1 = List("Operation failed.")
            val lifted = Free.lift[Either[List[String], *], Int](Left(error1))
            val impure = lifted.map(i => i * 2)
            val error2 = List("The other operation is also failed.")
            val f = (_: Int) => Free.lift[Either[List[String], *], Int](Left(error2)).map(_.toString)

            val actual = impure.flatMap(f)
            assertImpure[Either[List[String], *], Int, String](actual) { (fi, arrows) =>
              assert(fi == Left(error1))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1))
            }
          }

          "merges errors after flatMap" in {
            val lifted = Free.lift[Either[List[String], *], Int](Right(42))
            val impure = lifted.map(i => i * 2)
            val error1 = List("Operation failed.")
            val f = (_: Int) => Free.lift[Either[List[String], *], String](Left(error1))
            val error2 = List("The other operation is also failed.")
            val g = f.andThen(_.ap(Free.lift[Either[List[String], *], String => String](Left(error2))))

            val actual = impure.flatMap(g)
            assertImpure[Either[List[String], *], Int, String](actual) { (fi, arrows) =>
              assert(fi == Right(42))
              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              assert(arrows.run(fi) === Left(error1 ++ error2))
            }
          }
        }
      }

      "transform" - {
        "returns transformed Free" in {
          val impure1 = for {
            s <- Free.lift(Box("abc"))
            i <- Free.lift(Box(s.length))
            f <- Free.lift(Box((j: Int) => (i + j).toDouble))
          } yield f
          val impure = for {
            i <- Free.lift(Box(39))
            d <- Free.lift(Box(i.toDouble))
            f = (d: Double) => d.toString
            s <- Free.pure(d).ap(Free.lift(Box(f)))
            l <- Free.lift(Box(s.split('.').toList))
            o <- Free.pure(l.headOption)
            i2 <- Free.lift(Box(o.getOrElse("0").toInt))
            r <- Free.lift(Box(i2)).ap(impure1)
          } yield r

          val actual = impure.transform(BoxToOption)
          assertImpure[Option, Int, Double](actual) { (fi, arrows) =>
            assert(fi === Option(39))
            import types.Option.monad
            assert(arrows.run(fi) === Option(42d))
          }
        }
      }

      "extract" - {
        "returns computed value" in {
          val free1 = Free.lift(Option(40))
          val free2 = free1.flatMap(i => Free.lift(Option(i + 2)))
          val free3 = free2.ap(Free.lift(Option((i: Int) => i.toString)))
          val free4 = free3.ap(Free.lift(Option((s: String) => (i: Int) => s.length + i)))
          val free5 = Free.lift(Option(40)).ap(free4)
          val free6 = free5.flatMap(i => Free.lift(Option(i.toDouble)))
          val free7 = free6.flatMap(d => Free.lift(Option(d.toString)))
          val free8 = free7.flatMap(s => Free.lift(Option(s.split('.').toList)))
          val free9 = free8.flatMap(l => Free.lift(l.headOption))
          val free10 = free9.map(_.toInt)

          import types.Option.monad
          val actual = free10.extract
          assert(actual === Option(42))
        }

        "with None" - {
          "returns None" in {
            val free1 = Free.lift(Option(42))
            val free2 = Free.lift[Option, Int => Int](None)
            val free3 = free1.ap(free2).flatMap(i => Free.lift(Option(i + 1)))

            import types.Option.monad
            val actual = free3.extract
            assert(actual === None)
          }
        }

        "runnable with uneven lists" taggedAs Slow in {
          val f = (i: Int) => i % 3 == 0
          val g = (i: Int) => (i to i * 2).zipWithIndex.toList
          val h = (tp: (Int, Int)) => tp._1 + tp._2
          val i = (i: Int) => if (f(i)) List(i) else g(i).map(h)
          val j = (i: Int) => if (f(i)) Free.pure[List, Int](i) else Free.lift(g(i)).map(h)
          var list = List(1, 2, 3, 4)
          var free: Eff[List, Int] = Free.lift(list)
          for (_ <- 1 to 5) {
            list = list.flatMap(i)
            free = free.flatMap(j)
          }

          import types.List.monad
          assert(free.extract === list)
        }

        "runnable with deep flatMaps" taggedAs Slow in {
          var free = Free.lift(Box(0))
          for (_ <- 1 to 30000) {
            free = free.flatMap(i => Free.lift(Box(i + 1)))
          }

          import types.Box.monad
          assert(free.extract === Box(30000))
        }
      }

      "foldMap" - {
        "returns extracted instance" in {
          val free = for {
            i <- Free.lift(Box(20))
            j <- Free.lift(Box(i * 2))
            k <- Free.lift(Box(j + 2))
          } yield k

          import types.Option.monad
          assert(free.foldMap(BoxToOption) === Option(42))
        }
      }

      "flatten" - {
        "contains Free.Pure" - {
          "returns Free.Impure" in {
            val f = (i: Int) => Free.pure[Box, String]((i + 1).toString)
            val inner = f(41)
            assertPure(inner)("42")

            val outer = Free.lift(Box(41)).map(f)
            val actual = outer.flatten
            assertImpure[Box, Int, String](actual) { (fi, arrows) =>
              assert(fi === Box(41))
              import types.Box.monad
              assert(arrows.run(fi) === Box("42"))
            }
          }
        }

        "contains Free.Lifted" - {
          "returns Free.Impure" in {
            val f = (i: Int) => Free.lift(Box((i + 1).toString))
            val inner = f(41)
            assertLifted[Box, String](inner) { fi =>
              assert(fi === Box("42"))
            }

            val outer = Free.lift(Box(41)).map(f)
            val actual = outer.flatten
            assertImpure[Box, Int, String](actual) { (fi, arrows) =>
              assert(fi === Box(41))
              import types.Box.monad
              assert(arrows.run(fi) === Box("42"))
            }
          }
        }

        "contains Free.Impure" - {
          "returns Free.Impure" in {
            val f = (s: String) => Free.lift(Box((i: Int) => Free.lift(Box(s.length + i))))
            val ba = Box("a")
            val inner = Free.lift(ba).flatMap(f)
            val bi = Box(41)
            val outer = Free.lift(bi).ap(inner)

            val actual = outer.flatten
            assertImpure[Box, Int, Int](actual) { (fi, arrows) =>
              assert(fi === bi)
              import types.Box.monad
              assert(arrows.run(fi) === Box(42))
            }
          }
        }
      }
    }

    "apply" - {
      "Free.Pure" - {
        "returns Free.Impure" in {
          val impure = Free.lift(Box("abc")).map(s => (i: Int) => (s.length + i).toDouble)
          val pure = Free.pure[Box, Int](39)
          assertImpure[Box, String, Double](impure(pure)) { (fi, arrows) =>
            assert(fi === Box("abc"))
            import types.Box.monad
            assert(arrows.run(fi) === Box(42d))
          }
        }

        "keeps error on Free.Lifted" in {
          val error: Either[List[String], String] = Left(List("Error occurred."))
          val lifted = Free.lift(error).map(s => (i: Int) => (s.length + i).toDouble)
          val pure = Free.pure[Either[List[String], *], Int](42)
          assertImpure[Either[List[String], *], String, Double](lifted(pure)) { (fi, arrows) =>
            assert(fi === error)
            import types.Either.monadWithLeftSemigroup
            import types.List.semigroup
            assert(arrows.run(fi) === error)
          }
        }
      }

      "Free.Lifted" - {
        "returns Free.Impure" in {
          val box = Box("abc")
          val lifted1 = Free.lift(box).map(s => (i: Int) => (s.length + i).toDouble)
          val lifted2 = Free.lift(Box(39))
          assertImpure[Box, String, Double](lifted1(lifted2)) { (fi, arrows) =>
            assert(fi === box)
            import types.Box.monad
            assert(arrows.run(fi) === Box(42d))
          }
        }

        "merges errors in Free" in {
          val error1 = List("First error occurred.")
          val error2 = List("Another error occurred.")
          val error3 = List("Even other error occurred.")
          val either1: Either[List[String], String] = Left(error1)
          val either2: Either[List[String], String => Int => Double] = Left(error2)
          val either3: Either[List[String], Int] = Left(error3)
          val impure = Free.lift(either1).ap(Free.lift(either2))
          val lifted = Free.lift(either3)
          assertImpure[Either[List[String], *], String, Double](impure(lifted)) { (fi, arrows) =>
            assert(fi === either1)
            import types.Either.monadWithLeftSemigroup
            import types.List.semigroup
            assert(arrows.run(fi) === Left(error1 ++ error2 ++ error3))
          }
        }
      }

      "Free.Impure" - {
        "returns Free.Impure" in {
          val box = Box("abc")
          val impure1 = Free.lift(box).map(s => (i: Int) => (s.length + i).toDouble)
          val impure2 = Free.lift(Box("xyz")).map(_.length)
          assertImpure[Box, String, Double](impure1(impure2)) { (fi, arrows) =>
            assert(fi === box)
            import types.Box.monad
            assert(arrows.run(fi) === Box(6d))
          }
        }

        "merges errors in Free" in {
          val error1 = List("1st error occurred.")
          val error2 = List("2nd error occurred.")
          val error3 = List("3rd error occurred.")
          val error4 = List("4th error occurred.")
          val either1: Either[List[String], String] = Left(error1)
          val either2: Either[List[String], String => Int => Double] = Left(error2)
          val either3: Either[List[String], Double] = Left(error3)
          val either4: Either[List[String], Double => Int] = Left(error4)
          val impure = Free.lift(either1).ap(Free.lift(either2))
          val lifted = Free.lift(either3).ap(Free.lift(either4))
          assertImpure[Either[List[String], *], String, Double](impure(lifted)) { (fi, arrows) =>
            assert(fi === either1)
            import types.Either.monadWithLeftSemigroup
            import types.List.semigroup
            assert(arrows.run(fi) === Left(error1 ++ error2 ++ error3 ++ error4))
          }
        }
      }
    }
  }
}
