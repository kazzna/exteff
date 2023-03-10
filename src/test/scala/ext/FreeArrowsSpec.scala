package ext

import ext.transform.{BoxToOption, OptionToList}
import org.scalatest.Assertion
import org.scalatest.EitherValues.{convertEitherToValuable, convertLeftProjectionToValuable}
import org.scalatest.freespec.AnyFreeSpec
import test.Box

class FreeArrowsSpec extends AnyFreeSpec {
  def assertPure[F[_], A](free: Free[F, A])(
      assert: A => Assertion
  ): Assertion = free match {
    case Free.Pure(a) => assert(a)
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

  def assertSingle[F[_], G[_], A, B](arrows: Free.Arrows[G, A, B])(
      assert: (Free.Arrows.Arrow[F, A, B], NaturalTransformation[F, G]) => Assertion
  ): Assertion = arrows match {
    case single @ Free.Arrows.Single(_, _) =>
      assert(
        single.arrow.asInstanceOf[Free.Arrows.Arrow[F, A, B]],
        single.nt.asInstanceOf[NaturalTransformation[F, G]]
      )
    case arrows => fail(s"Free.Arrows.Single expected but $arrows found.")
  }
  def assertMap[F[_], G[_], A, B](arrows: Free.Arrows[G, A, B])(
      assert: (A => B, NaturalTransformation[F, G]) => Assertion
  ): Assertion = assertSingle[F, G, A, B](arrows) { (arrow, nt) =>
    arrow match {
      case Free.Arrows.Arrow.Map(f) => assert(f, nt)
      case arrow => fail(s"Free.Arrows.Arrow.Map expected but $arrow found.")
    }
  }
  def assertLift[F[_], G[_], A, B](arrows: Free.Arrows[G, A, B])(
      assert: (F[A => B], NaturalTransformation[F, G]) => Assertion
  ): Assertion = assertSingle[F, G, A, B](arrows) { (arrow, nt) =>
    arrow match {
      case Free.Arrows.Arrow.Lift(f) => assert(f, nt)
      case arrow => fail(s"Free.Arrows.Arrow.Lift expected but $arrow found.")
    }
  }
  def assertApply[F[_], G[_], I, A, B](arrows: Free.Arrows[G, A, B])(
      assert: (F[I], Free.Arrows[F, I, A => B], NaturalTransformation[F, G]) => Assertion
  ): Assertion = assertSingle[F, G, A, B](arrows) { (arrow, nt) =>
    arrow match {
      case apply @ Free.Arrows.Arrow.Apply(_, _) =>
        assert(
          apply.fi.asInstanceOf[F[I]],
          apply.arrows.asInstanceOf[Free.Arrows[F, I, A => B]],
          nt
        )
      case arrow => fail(s"Free.Arrows.Arrow.Apply expected but $arrow found.")
    }
  }
  def assertBind[F[_], G[_], I[_], A, B](arrows: Free.Arrows[I, A, B])(
      assert: (A => Free[F, B], NaturalTransformation[F, G], NaturalTransformation[G, I]) => Assertion
  ): Assertion = assertSingle[G, I, A, B](arrows) { (arrow, nt) =>
    arrow match {
      case bind @ Free.Arrows.Arrow.Bind(_, _) =>
        assert(
          bind.f.asInstanceOf[A => Free[F, B]],
          bind.nt.asInstanceOf[NaturalTransformation[F, G]],
          nt
        )
      case arrow => fail(s"Free.Arrows.Arrow.Bind expected but $arrow found.")
    }
  }
  def assertComposed[F[_], G[_], A, B, C](arrows: Free.Arrows[G, A, C])(
      assert: (Free.Arrows[F, A, B], Free.Arrows[F, B, C], NaturalTransformation[F, G]) => Assertion
  ): Assertion = arrows match {
    case composed @ Free.Arrows.Composed(_, _, _) =>
      assert(
        composed.arrows1.asInstanceOf[Free.Arrows[F, A, B]],
        composed.arrows2.asInstanceOf[Free.Arrows[F, B, C]],
        composed.nt.asInstanceOf[NaturalTransformation[F, G]]
      )
    case arrows => fail(s"Free.Arrows.Single expected but $arrows found.")
  }

  "Free.Arrows object" - {
    "map" - {
      "creates Arrows.Single contains Arrow.Map" in {
        val f = (i: Int) => i.toLong

        val actual = Free.Arrows.map[Box, Int, Long](f)
        assertMap[Box, Box, Int, Long](actual) { (g, nt) =>
          assert(g === f)
          assert(nt === NaturalTransformation.reflect)
        }
      }
    }

    "lift" - {
      "creates Arrows.Single contains Arrow.Lift" in {
        val f = Box((i: Int) => i + 1)

        val actual = Free.Arrows.lift(f)
        assertLift[Box, Box, Int, Int](actual) { (g, nt) =>
          assert(g === f)
          assert(nt === NaturalTransformation.reflect)
        }
      }
    }

    "apply" - {
      "Free.Pure" - {
        "creates Arrows.Single contains Arrow.Map" in {
          val f = (i: Int) => i.toString
          val pure = Free.point[Box, Int => String](f)

          val actual = Free.Arrows.apply(pure)
          assertMap[Box, Box, Int, String](actual) { (g, nt) =>
            assert(g === f)
            assert(nt === NaturalTransformation.reflect)
          }
        }
      }

      "Free.Lifted" - {
        "creates Arrows.Single contains Arrow.Lift" in {
          val f = Box((i: Int) => i.toDouble)
          val lifted = Free.lift(f)

          val actual = Free.Arrows.apply(lifted)
          assertLift[Box, Box, Int, Double](actual) { (g, nt) =>
            assert(g === f)
            assert(nt === NaturalTransformation.reflect)
          }
        }
      }

      "Free.Impure" - {
        "creates Arrows.Single contains Arrow.Apply" in {
          val bf = Box((i: Int) => i.toString)
          val f = (g: Int => String) => Free.lift(Box(g.andThen(_.length)))
          val impure = Free.lift(bf).flatMap(f)

          val actual = Free.Arrows.apply(impure)
          assertApply[Box, Box, Int => String, Int, Int](actual) { (fi, arrows, nt) =>
            assert(fi === bf)
            assert(arrows === Free.Arrows.bind(f))
            assert(nt === NaturalTransformation.reflect)
          }
        }
      }
    }

    "bind" - {
      "creates Arrows.Single contains Arrow.Bind" in {
        val f = (i: Int) => Free.point[Box, Int](i + 1)

        val actual = Free.Arrows.bind(f)
        assertBind[Box, Box, Box, Int, Int](actual) { (g, nt0, nt1) =>
          assert(g == f)
          assert(nt0 === NaturalTransformation.reflect)
          assert(nt1 === NaturalTransformation.reflect)
        }
      }
    }
  }

  "Free.Arrows instance" - {
    "Single" - {
      "with Arrows.Arrow.Map" - {
        "apply" - {
          "returns Free.Pure" in {
            val arrows = Free.Arrows.map[Option, Int, Int](_ + 1)

            val actual = arrows(41)
            assert(actual === Free.point(42))
          }
        }

        "compose" - {
          "returns Arrows.Composed" in {
            val map = Free.Arrows.map[Box, Int, Int](_ + 1)
            val arrows = Free.Arrows.bind((i: Int) => Free.lift(Box(i + 1)))

            val actual = map.compose(arrows)
            assertComposed[Box, Box, Int, Int, Int](actual) { (arrows1, arrows2, nt) =>
              assert(arrows1 === map)
              assert(arrows2 === arrows)
              assert(nt === NaturalTransformation.reflect)
            }
          }
        }

        "transform" - {
          "returns new Map" in {
            val map = Free.Arrows.map[Option, Int, Int](_ + 1)

            val actual = map.transform(OptionToList)
            assert(actual.apply(41) === Free.pure[List, Int](42))
          }
        }

        "resume" - {
          "returns mapped F[B]" in {
            val map = Free.Arrows.map[Option, Int, Double](_.toDouble)

            import types.Option.monad
            assert(map.resume(Option(42)).value === Option(42d))
          }
        }
      }

      "with Arrows.Arrow.Lift" - {
        "apply" - {
          "returns Free.Impure" in {
            import types.Box.monad
            val f = (i: Int) => i + 1
            val bf = Box(f)
            val arrows = Free.Arrows.lift(bf)

            val actual = arrows(41)
            assert(actual.extract === Box(42))
          }
        }

        "compose" - {
          "returns Arrows.Composed" in {
            val lift = Free.Arrows.lift(Box((i: Int) => i + 1))
            val arrows = Free.Arrows.bind((i: Int) => Free.lift(Box(i + 1)))

            val actual = lift.compose(arrows)
            assertComposed[Box, Box, Int, Int, Int](actual) { (arrows1, arrows2, nt) =>
              assert(arrows1 === lift)
              assert(arrows2 === arrows)
              assert(nt === NaturalTransformation.reflect)
            }
          }
        }

        "transform" - {
          "returns new Lift" in {
            val lift = Free.Arrows.lift[Option, Int, Int](Option(_ + 1))

            val actual = lift.transform(OptionToList)
            import types.List.monad
            assert(actual.run(List(41)) === List(42))
          }
        }

        "resume" - {
          "returns applied F[B]" in {
            val lift = Free.Arrows.lift[Option, Int, Double](Option(_.toDouble))

            import types.Option.monad
            assert(lift.resume(Some(42)).value === Option(42d))
          }
        }
      }

      "with Arrows.Arrow.Apply" - {
        "apply" - {
          "returns Free.Impure" in {
            val a = "a"
            val ba = Box(a)
            val fa = Free.lift(ba)
            val f = (s: String) => (i: Int) => s.length + i
            val bf = Box(f)
            val ff = Free.lift(bf)
            val free = fa.ap(ff)
            val arrows = Free.Arrows.apply(free)

            val actual = arrows(41)
            assertImpure[Box, String, Int](actual) { (fi, arrows) =>
              import types.Box.monad
              assert(fi === ba)
              assert(arrows.run(fi) === Box(42))
            }
          }
        }

        "compose" - {
          "returns Arrows.Composed" in {
            val fa = Free.lift(Box("a"))
            val ff = Free.lift(Box((s: String) => (i: Int) => s.length + i))
            val apply = Free.Arrows.apply(fa.ap(ff))
            val arrows = Free.Arrows.bind((i: Int) => Free.lift(Box(i + 1)))

            val actual = apply.compose(arrows)
            assertComposed[Box, Box, Int, Int, Int](actual) { (arrows1, arrows2, nt) =>
              assert(arrows1 === apply)
              assert(arrows2 === arrows)
              assert(nt === NaturalTransformation.reflect)
            }
          }
        }

        "transform" - {
          "returns new Apply" in {
            val free1 = (s: String) =>
              for {
                i <- Free.lift(Box(s.length))
                f <- Free.lift(Box((j: Int) => i + j))
              } yield f
            val free2 = Free.point[Box, String]("abc").flatMap(free1)
            val apply = Free.Arrows.apply(free2)

            val actual = apply.transform(BoxToOption)
            import types.Option.monad
            assert(actual.run(Option(39)) === Option(42))
          }
        }

        "without Arrows.Arrow.Bind" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val f = (s: String) => (i: Int) => (s.length + i).toDouble
              val free = Free.lift(Option("abc")).ap(Free.lift(Option(f)))
              val apply = Free.Arrows.apply(free)

              import types.Option.monad
              val actual = apply.resume(Option(39))
              assertImpure[Option, (Int, String), Double](actual.left.value.value) { (fi, arrows) =>
                assert(fi === Option((39, "abc")))
                assert(arrows.run(fi) === Option(42))
              }
            }

            "keeps errors' order" in {
              val f = (s: String) => (i: Int) => (s.length + i).toDouble
              val es: Either[List[String], String] = Left(List("2nd"))
              val ef: Either[List[String], String => Int => Double] = Right(f)
              val free1 = Free.lift(es)
              val free2 = Free.lift(ef)
              val free3 = free1.ap(free2)
              val apply = Free.Arrows.apply(free3)

              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              val actual = apply.resume(Left(List("1st")))
              assertImpure[Either[List[String], *], (Int, String), Double](actual.left.value.value) { (fi, arrows) =>
                assert(fi === Left(List("1st", "2nd")))
                assert(arrows.run(fi) === Left(List("1st", "2nd")))
              }
            }
          }
        }

        "includes Arrows.Arrow.Bind" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val f = (s: String) => Free.lift(Option((i: Int) => (s.length + i).toDouble))
              val free = Free.lift(Option("abc")).flatMap(f)
              val apply = Free.Arrows.apply(free)

              import types.Option.monad
              val actual = apply.resume(Option(39))
              assertImpure[Option, (Int, String), Double](actual.left.value.value) { (fi, arrows) =>
                assert(fi === Option("abc"))
                assert(arrows.run(fi) === Option(42))
              }
            }

            "keeps errors' order" - {
              val es: Either[List[String], String] = Left(List("2nd"))
              val f: String => Free[Either[List[String], *], Int => Double] = { s =>
                Free.lift(Right(i => (s.length + i).toDouble))
              }
              val free = Free.lift(es).flatMap(f)
              val apply = Free.Arrows.apply(free)

              import types.Either.monadWithLeftSemigroup
              import types.List.semigroup
              val actual = apply.resume(Left(List("1st")))
              assertImpure[Either[List[String], *], (Int, String), Double](actual.left.value.value) { (fi, arrows) =>
                assert(fi === Left(List("1st", "2nd")))
                assert(arrows.run(fi) === Left(List("1st", "2nd")))
              }
            }
          }
        }
      }

      "with Arrows.Arrow.Bind" - {
        "apply" - {
          "returns Free.Pure" in {
            val f = (i: Int) => Free.lift(Box(i.toDouble))
            val arrows = Free.Arrows.bind(f)

            val actual = arrows(42)
            assert(actual === Free.lift(Box(42d)))
          }
        }

        "compose" - {
          "returns Arrows.Composed" in {
            val f = (i: Int) => Free.lift(Box(i.toDouble))
            val bind = Free.Arrows.bind(f)
            val arrows = Free.Arrows.bind((d: Double) => Free.lift(Box(d + 1)))

            val actual = bind.compose(arrows)
            assertComposed[Box, Box, Int, Double, Double](actual) { (arrows1, arrows2, nt) =>
              assert(arrows1 === bind)
              assert(arrows2 === arrows)
              assert(nt === NaturalTransformation.reflect)
            }
          }
        }

        "transform" - {
          "returns Bind with transform" in {
            import types.List.monad
            val f = (i: Int) => Free.lift(Option(i + 1))
            val arrows = Free.Arrows.bind(f)

            val actual = arrows.transform(OptionToList)
            assert(actual.run(List(41)) === List(42))
          }
        }

        "resume" - {
          "returns left left F[Free[F, B]]" in {
            val f = (s: String) => Free.lift(Option((s.length + 39).toDouble))
            val bind = Free.Arrows.bind(f)

            import types.Option.monad
            val actual = bind.resume(Option("abc"))
            implicitly[types.Functor[Option]].map(actual.left.value.left.value) { free =>
              assertLifted[Option, Double](free) { fi =>
                assert(fi === Option(42d))
              }
            }
          }
        }
      }
    }

    "Composed" - {
      "apply" - {
        "returns Free.Impure" in {
          val arrows1 = Free.Arrows.bind((i: Int) => Free.lift(Box(i + 1)))
          val arrows2 = Free.Arrows.bind((i: Int) => Free.lift(Box(i.toDouble)))
          val arrows3 = Free.Arrows.bind((d: Double) => Free.lift(Box(d.toString)))
          val arrows4 = Free.Arrows.bind((s: String) => Free.lift(Box(s.split('.'))))
          val arrows5 = Free.Arrows.bind((array: Array[String]) => Free.lift(Box(array.toList)))
          val arrows6 = Free.Arrows.bind((list: List[String]) => Free.lift(Box(list.map(_.toInt))))

          val composed1 = arrows1.compose(arrows2).compose(arrows3)
          val composed2 = composed1.compose(arrows4).compose(arrows5).compose(arrows6)

          val actual = composed2(41)
          assertImpure[Box, Int, List[Int]](actual) { (fi, arrows) =>
            import types.Box.monad
            assert(fi === Box(42))
            assert(arrows.run(fi) === Box(List(42, 0)))
          }
        }
      }

      "compose" - {
        "returns Arrows.Composed" in {
          val a1 = Free.Arrows.map[Box, Int, Double](_.toDouble)
          val a2 = Free.Arrows.lift(Box((d: Double) => d.toString))
          val composed = a1.compose(a2)
          val arrows = Free.Arrows.bind((s: String) => Free.lift(Box(s.split('.'))))

          val actual = composed.compose(arrows)
          assertComposed[Box, Box, Int, String, Array[String]](actual) { (arrows1, arrows2, nt) =>
            assert(arrows1 === composed)
            assert(arrows2 === arrows)
            assert(nt === NaturalTransformation.reflect)
          }
        }
      }

      "transform" - {
        "returns transformed Arrows.Composed" in {
          val a1 = Free.Arrows.map[Box, Int, Double](_.toDouble)
          val a2 = Free.Arrows.lift(Box((d: Double) => d.toString))
          val left = a1.compose(a2)
          val right = Free.Arrows.bind((s: String) => Free.lift(Box(s.split('.'))))
          val composed = left.compose(right)

          val actual = composed.transform(BoxToOption)
          assertComposed[Box, Option, Int, String, Array[String]](actual) { (arrows1, arrows2, nt) =>
            assert(arrows1 === left)
            assert(arrows2 === right)
            assert(nt === BoxToOption)
          }
        }
      }

      "with Arrows.Arrow.Map head" - {
        "with Arrows.Arrow.Map tail" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val map = Free.Arrows.map[Box, Int, Double](_.toDouble)
              val f = (d: Double) => d.toString
              val map2 = Free.Arrows.map[Box, Double, String](f)
              val composed = map.compose(map2)

              import types.Box.monad
              val resumed = composed.resume(Box(42))
              assertImpure[Box, Double, String](resumed.left.value.value) { (fi, arrows) =>
                assert(fi === Box(42d))
                assert(arrows.run(fi) === Box("42.0"))
              }
            }
          }
        }

        "with Arrows.Arrow.Lift tail" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val map = Free.Arrows.map[Box, Int, Double](_.toDouble)
              val box = Box((d: Double) => d.toString)
              val lift = Free.Arrows.lift(box)
              val composed = map.compose(lift)

              import types.Box.monad
              val resumed = composed.resume(Box(42))
              assertImpure[Box, Double, String](resumed.left.value.value) { (fi, arrows) =>
                assert(fi === Box(42d))
                assert(arrows.run(fi) === Box("42.0"))
              }
            }
          }
        }

        "with Arrows.Arrow.Apply tail" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val map = Free.Arrows.map[Box, String, Int](_.length)
              val box = Box(39)
              val free = Free.lift(box).map(i => (j: Int) => (i + j).toDouble)
              val apply = Free.Arrows.apply(free)
              val composed = map.compose(apply)

              import types.Box.monad
              val resumed = composed.resume(Box("abc"))
              assertImpure[Box, Int, Double](resumed.left.value.value) { (fi, arrows) =>
                assert(fi === Box(3))
                assert(arrows.run(fi) === Box(42d))
              }
            }
          }
        }

        "with Arrows.Arrow.Bind tail" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val map = Free.Arrows.map[Box, String, Int](_.length)
              val f = (i: Int) => Free.lift(Box((i + 39).toDouble))
              val bind = Free.Arrows.bind(f)
              val composed = map.compose(bind)

              import types.Box.monad
              val resumed = composed.resume(Box("abc"))
              assertImpure[Box, Int, Double](resumed.left.value.value) { (fi, arrows) =>
                assert(fi === Box(3))
                assert(arrows.run(fi) === Box(42d))
              }
            }
          }
        }
      }

      "with Arrows.Arrow.Lift head" - {
        "with Arrows.Arrow.Map tail" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val box = Box[Int => Double](_.toDouble)
              val lift = Free.Arrows.lift(box)
              val f = (d: Double) => d.toString
              val map = Free.Arrows.map[Box, Double, String](f)
              val composed = lift.compose(map)

              import types.Box.monad
              val resumed = composed.resume(Box(42))
              assertImpure[Box, Double, String](resumed.left.value.value) { (fi, arrows) =>
                assert(fi === Box(42d))
                assert(arrows.run(fi) === Box("42.0"))
              }
            }
          }
        }

        "with Arrows.Arrow.Lift tail" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val box = Box[Int => Double](_.toDouble)
              val lift1 = Free.Arrows.lift(box)
              val f = Box((d: Double) => d.toString)
              val lift2 = Free.Arrows.lift(f)
              val composed = lift1.compose(lift2)

              import types.Box.monad
              val resumed = composed.resume(Box(42))
              assertImpure[Box, Double, String](resumed.left.value.value) { (fi, arrows) =>
                assert(fi === Box(42d))
                assert(arrows.run(fi) === Box("42.0"))
              }
            }
          }
        }

        "with Arrows.Arrow.Apply tail" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val box = Box[String => Int](_.length)
              val lift = Free.Arrows.lift(box)
              val free = Free.lift(Box(39)).map(i => (j: Int) => (i + j).toDouble)
              val apply = Free.Arrows.apply(free)
              val composed = lift.compose(apply)

              import types.Box.monad
              val resumed = composed.resume(Box("abc"))
              assertImpure[Box, Int, Double](resumed.left.value.value) { (fi, arrows) =>
                assert(fi === Box(3))
                assert(arrows.run(fi) === Box(42d))
              }
            }
          }
        }

        "with Arrows.Arrow.Bind tail" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val box = Box[String => Int](_.length)
              val lift = Free.Arrows.lift(box)
              val f = (i: Int) => Free.lift(Box((39 + i).toDouble))
              val bind = Free.Arrows.bind(f)
              val composed = lift.compose(bind)

              import types.Box.monad
              val resumed = composed.resume(Box("abc"))
              assertImpure[Box, Int, Double](resumed.left.value.value) { (fi, arrows) =>
                assert(fi === Box(3))
                assert(arrows.run(fi) === Box(42d))
              }
            }
          }
        }
      }

      "with Arrows.Arrow.Apply head" - {
        "with Arrows.Arrow.Map tail" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val box = Box("abc")
              val free = Free.lift(box).map(s => (j: Int) => (s.length + j).toDouble)
              val apply = Free.Arrows.apply(free)
              val f = Box((d: Double) => d.toString)
              val lift = Free.Arrows.lift(f)
              val composed = apply.compose(lift)

              import types.Box.monad
              val resumed = composed.resume(Box(39))
              assertImpure[Box, (Int, String), String](resumed.left.value.value) { (fi, arrows) =>
                assert(fi === Box((39, "abc")))
                assert(arrows.run(fi) === Box("42.0"))
              }
            }
          }
        }

        "with Arrows.Arrow.Lift tail" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val box = Box("abc")
              val free = Free.lift(box).map(s => (j: Int) => (s.length + j).toDouble)
              val apply = Free.Arrows.apply(free)
              val f = (d: Double) => d.toString
              val map = Free.Arrows.map[Box, Double, String](f)
              val composed = apply.compose(map)

              import types.Box.monad
              val resumed = composed.resume(Box(39))
              assertImpure[Box, (Int, String), String](resumed.left.value.value) { (fi, arrows) =>
                assert(fi === Box((39, "abc")))
                assert(arrows.run(fi) === Box("42.0"))
              }
            }
          }
        }

        "with Arrows.Arrow.Apply tail" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val box1 = Box("abc")
              val free1 = Free.lift(box1).map(s => (j: Int) => (s.length + j).toDouble)
              val apply1 = Free.Arrows.apply(free1)
              val box2 = Box("xyz")
              val free2 = Free.lift(box2).map(s => (d: Double) => s + d.toString + s)
              val apply2 = Free.Arrows.apply(free2)
              val composed = apply1.compose(apply2)

              import types.Box.monad
              val resumed = composed.resume(Box(39))
              assertImpure[Box, (Int, String), String](resumed.left.value.value) { (fi, arrows) =>
                assert(fi === Box((39, "abc")))
                assert(arrows.run(fi) === Box("xyz42.0xyz"))
              }
            }
          }
        }

        "with Arrows.Arrow.Bind tail" - {
          "resume" - {
            "returns left right Free[F, B]" in {
              val box = Box("abc")
              val free = Free.lift(box).map(s => (j: Int) => (s.length + j).toDouble)
              val apply = Free.Arrows.apply(free)
              val f = (d: Double) => Free.lift(Box(d.toString))
              val bind = Free.Arrows.bind(f)
              val composed = apply.compose(bind)

              import types.Box.monad
              val resumed = composed.resume(Box(39))
              assertImpure[Box, (Int, String), String](resumed.left.value.value) { (fi, arrows) =>
                assert(fi === Box((39, "abc")))
                assert(arrows.run(fi) === Box("42.0"))
              }
            }
          }
        }
      }

      "with Arrows.Arrow.Bind head" - {
        "with Arrows.Arrow.Map tail" - {
          "resume" - {
            "returns left left F[Free[F, B]]" in {
              val f = (s: String) => Free.lift(Option(s.length + 39))
              val bind = Free.Arrows.bind(f)
              val map = Free.Arrows.map[Option, Int, Double](_.toDouble)
              val composed = bind.compose(map)

              import types.Option.monad
              val resumed = composed.resume(Option("abc"))

              val option = resumed.left.value.left.value
              assert(option.isDefined)

              val free = option.getOrElse(Free.point(0d))
              assertImpure[Option, Int, Double](free) { (fi, arrows) =>
                assert(fi === Some(42))
                assert(arrows.run(fi) === Some(42d))
              }
            }
          }
        }

        "with Arrows.Arrow.Lift tail" - {
          "resume" - {
            "returns left left F[Free[F, B]]" in {
              val f = (s: String) => Free.lift(Option(s.length + 39))
              val bind = Free.Arrows.bind(f)
              val lift = Free.Arrows.lift[Option, Int, Double](Option(_.toDouble))
              val composed = bind.compose(lift)

              import types.Option.monad
              val resumed = composed.resume(Option("abc"))

              val option = resumed.left.value.left.value
              assert(option.isDefined)

              val free = option.getOrElse(Free.point(0d))
              assertImpure[Option, Int, Double](free) { (fi, arrows) =>
                assert(fi === Some(42))
                assert(arrows.run(fi) === Some(42d))
              }
            }
          }
        }

        "with Arrows.Arrow.Apply tail" - {
          "resume" - {
            "returns left left F[Free[F, B]]" in {
              val f = (s: String) => Free.lift(Option(s.length + 36))
              val bind = Free.Arrows.bind(f)
              val os = Option("xyz")
              val free = Free.lift(os).map(s => (i: Int) => (s.length + i).toDouble)
              val apply = Free.Arrows.apply(free)
              val composed = bind.compose(apply)

              import types.Option.monad
              val resumed = composed.resume(Option("abc"))

              val option = resumed.left.value.left.value
              assert(option.isDefined)

              assertImpure[Option, Int, Double](option.getOrElse(Free.point(0d))) { (fi, arrows) =>
                assert(fi === Some(39))
                assert(arrows.run(fi) === Some(42d))
              }
            }
          }
        }

        "with Arrows.Arrow.Bind tail" - {
          "resume" - {
            "returns left left F[Free[F, B]]" in {
              val f = (s: String) => Free.lift(Option(s.length + 39))
              val bind1 = Free.Arrows.bind(f)
              val g = (i: Int) => Free.lift(Option(i.toDouble))
              val bind2 = Free.Arrows.bind(g)
              val composed = bind1.compose(bind2)

              import types.Option.monad
              val resumed = composed.resume(Option("abc"))

              val option = resumed.left.value.left.value
              assert(option.isDefined)

              assertImpure[Option, Int, Double](option.getOrElse(Free.point(0d))) { (fi, arrows) =>
                assert(fi === Some(42))
                assert(arrows.run(fi) === Some(42d))
              }
            }
          }
        }
      }
    }
  }
}
