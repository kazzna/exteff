package ext

import org.scalatest.freespec.AnyFreeSpec

class StateSpec extends AnyFreeSpec {
  type Stack[A] = List[A]

  def empty[A]: Stack[A] = List.empty

  def push[A](a: A): State[Stack[A], Unit] = State.modify(a +: _)

  def pop[A]: State[Stack[A], Option[A]] = for {
    stack <- State.get[Stack[A]]
    head = stack.headOption
    tail = head.map(_ => stack.tail).getOrElse(stack)
    _ <- State.put(tail)
  } yield head

  def swap[A]: State[Stack[A], Unit] = for {
    a <- pop[A]
    b <- pop
    _ <- a.map(push).getOrElse(State.point(()))
    _ <- b.map(push).getOrElse(State.point(()))
  } yield ()

  "State object" - {
    "pure" - {
      "returns State" in {
        val pure = State.pure[String, Int](42)

        assert(pure.run("s") === (42, "s"))
      }
    }

    "point" - {
      "returns State" in {
        val point = State.point[String, Int](42)

        assert(point.run("s") === (42, "s"))
      }
    }

    "get" - {
      "returns State which get State" in {
        val get = State.get[String]

        assert(get.run("abc") === ("abc", "abc"))
      }
    }

    "put" - {
      "returns State which put State" in {
        val put = State.put[String]("xyz")

        assert(put.run("abc") === ((), "xyz"))
      }
    }

    "modify" - {
      "returns State modifying input state" in {
        val modify = State.modify[Int](_ + 1)

        assert(modify.run(41) === ((), 42))
      }
    }

    "apply" - {
      "returns State" in {
        val apply = State.apply((s: String) => (s.length, s))

        assert(apply.run("abc") === (3, "abc"))
      }
    }
  }

  "State instance" - {
    "map" - {
      "returns State with new value" in {
        val state = State.get[Int]

        val actual = state.map(_ + 1)
        assert(actual.run(41) === (42, 41))
      }
    }

    "ap" - {
      "runs self state modification first" in {
        val state = for {
          _ <- push("before")
          s <- State.get
        } yield s.headOption.getOrElse("abc")

        val ap = for {
          stack <- State.get[Stack[String]]
          _ <- push("after")
        } yield (s: String) => s +: stack

        val actual = state.ap(ap)
        assert(actual.run(empty) === (List("before", "before"), List("after", "before")))
      }
    }

    "map2" - {
      "runs self state modification first" in {
        val state1 = for {
          _ <- push("before")
          s <- State.get
        } yield s
        val state2 = for {
          _ <- push("after")
          s <- State.get
        } yield s.headOption.getOrElse("abc")

        val actual = state1.map2(state2)((list, string) => list :+ string)
        assert(actual.run(empty) === (List("before", "after"), List("after", "before")))
      }
    }

    "flatMap" - {
      "runs self state modification first" in {
        val state = for {
          _ <- push("before")
          s <- State.get
        } yield s.headOption.getOrElse("abc")
        val f = (s: String) =>
          for {
            _ <- push("after")
            _ <- push(s"***$s***")
          } yield s.length

        val actual = state.flatMap(f)
        assert(actual.run(empty) === (6, List("***before***", "after", "before")))
      }
    }

    "run" - {
      "returns ran result value and state" in {
        val state = for {
          _ <- push(1)
          _ <- push(2)
          _ <- swap
          _ <- push(3)
          a <- pop
          _ <- push(4)
          b <- pop
          _ <- push(a.getOrElse(12))
        } yield b

        assert(state.run(empty) === (Option(4), List(3, 1, 2)))
      }
    }

    "eval" - {
      "returns ran result value" in {
        val state = for {
          _ <- push("abc")
          _ <- push("def")
          a <- pop
        } yield a.getOrElse("xyz")

        assert(state.eval(empty) === "def")
      }
    }

    "exec" - {
      "returns ran result state" in {
        val state = for {
          a <- pop[String]
        } yield a

        assert(state.exec(List("abc", "def", "ghi")) === List("def", "ghi"))
      }
    }
  }
}
