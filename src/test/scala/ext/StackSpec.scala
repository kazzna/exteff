package ext

import org.scalatest.freespec.AnyFreeSpec

class StackSpec extends AnyFreeSpec {
  "Stack" - {
    "of1" - {
      "extract" - {
        "extracts top container" in {
          val of1: Stack.of1[Option]#R[Int] = Stack.top(Option(42))
          val expected = Option(42)

          assert(of1.extract == expected)
        }
      }
    }

    "of2" - {
      "restructure" - {
        "restructures top container" in {
          val of2: Stack.of2[Option, List]#R[Int] = Stack.top(Option(42))
          val expected: Stack.of1[List]#R[Int] = Stack.top(List(42))

          val nt = new NaturalTransformation[Option, List] {
            override def apply[A](fa: Option[A]): List[A] = fa.toList
          }

          assert(of2.restructure(nt) == expected)
        }
      }
    }

    "of3" - {
      "restructure" - {
        "restructures top container" in {
          type Of3[A] = Stack.of3[Option, Int => *, List]#R[A]
          type Of2[A] = Stack.of2[Int => *, List]#R[A]
          val of3: Of3[Int] = Stack.top(Option(42))
          val expected: Of2[Int] = Stack.body(Stack.top(List(42)))

          val nt = new NaturalTransformation[Option, List] {
            override def apply[A](fa: Option[A]): List[A] = fa.toList
          }

          assert(of3.restructure(nt) == expected)
        }
      }
    }

    "of4" - {
      "restructure" - {
        "restructures top container" in {
          type Of4[A] = Stack.of4[Option, Either[String, *], Int => *, List]#R[A]
          type Of3[A] = Stack.of3[Either[String, *], Int => *, List]#R[A]
          val of4: Of4[Int] = Stack.top(Option(42))
          val expected: Of3[Int] = Stack.body(Stack.body(Stack.top(List(42))))

          val nt = new NaturalTransformation[Option, List] {
            override def apply[A](fa: Option[A]): List[A] = fa.toList
          }

          assert(of4.restructure(nt) == expected)
        }
      }
    }

    "of5" - {
      "restructure" - {
        "restructures top container" in {
          case class Box[A](a: A)
          type Of5[A] = Stack.of5[Option, Box, Either[String, *], Int => *, List]#R[A]
          type Of4[A] = Stack.of4[Box, Either[String, *], Int => *, List]#R[A]
          val of5: Of5[Int] = Stack.top(Option(42))
          val stack: List In Of4 = implicitly
          val expected: Of4[Int] = stack.inject(List(42))

          val nt = new NaturalTransformation[Option, List] {
            override def apply[A](fa: Option[A]): List[A] = fa.toList
          }

          assert(of5.restructure(nt) == expected)
        }
      }
    }

    "of6" - {
      "restructure" - {
        "restructures top container" in {
          case class Box1[A](a: A)
          case class Box2[A](a: A)
          type Of6[A] = Stack.of6[Option, Box1, Box2, Either[String, *], Int => *, List]#R[A]
          type Of5[A] = Stack.of5[Box1, Box2, Either[String, *], Int => *, List]#R[A]
          val of6: Of6[Int] = Stack.top(Option(42))
          val stack: List In Of5 = implicitly
          val expected: Of5[Int] = stack.inject(List(42))

          val nt = new NaturalTransformation[Option, List] {
            override def apply[A](fa: Option[A]): List[A] = fa.toList
          }

          assert(of6.restructure(nt) == expected)
        }
      }
    }

    "of7" - {
      "restructure" - {
        "restructures top container" in {
          case class Box1[A](a: A)
          case class Box2[A](a: A)
          case class Box3[A](a: A)
          type Of7[A] = Stack.of7[Option, Box1, Box2, Box3, Either[String, *], Int => *, List]#R[A]
          type Of6[A] = Stack.of6[Box1, Box2, Box3, Either[String, *], Int => *, List]#R[A]
          val of7: Of7[Int] = Stack.top(Option(42))
          val stack: List In Of6 = implicitly
          val expected: Of6[Int] = stack.inject(List(42))

          val nt = new NaturalTransformation[Option, List] {
            override def apply[A](fa: Option[A]): List[A] = fa.toList
          }

          assert(of7.restructure(nt) == expected)
        }
      }
    }

    "of8" - {
      "restructure" - {
        "restructures top container" in {
          case class Box1[A](a: A)
          case class Box2[A](a: A)
          case class Box3[A](a: A)
          case class Box4[A](a: A)
          type Of8[A] = Stack.of8[Option, Box1, Box2, Box3, Box4, Either[String, *], Int => *, List]#R[A]
          type Of7[A] = Stack.of7[Box1, Box2, Box3, Box4, Either[String, *], Int => *, List]#R[A]
          val of8: Of8[Int] = Stack.top(Option(42))
          val stack: List In Of7 = implicitly
          val expected: Of7[Int] = stack.inject(List(42))

          val nt = new NaturalTransformation[Option, List] {
            override def apply[A](fa: Option[A]): List[A] = fa.toList
          }

          assert(of8.restructure(nt) == expected)
        }
      }
    }
  }
}
