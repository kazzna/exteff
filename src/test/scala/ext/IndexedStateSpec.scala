package ext

import org.scalatest.freespec.AnyFreeSpec

class IndexedStateSpec extends AnyFreeSpec {
  type Stack[A] = List[A]

  def empty[A]: Stack[A] = List.empty
  def push[A](a: A): IndexedState[Stack[A], Stack[A], Unit] = for {
    stack <- IndexedState.get[Stack[A]]
    _ <- IndexedState.put(a +: stack)
  } yield ()
  def pop[A]: IndexedState[Stack[A], Stack[A], A] = for {
    stack <- IndexedState.get[Stack[A]]
    a = stack.head
    stack2 = stack.tail
    _ <- IndexedState.put(stack2)
  } yield a
  def swap[A]: IndexedState[Stack[A], Stack[A], Unit] = for {
    a1 <- pop[A]
    a2 <- pop
    _ <- push(a2)
    _ <- push(a1)
  } yield ()
  def sumTop2[A: Numeric]: IndexedState[Stack[A], Stack[A], A] = for {
    a1 <- pop[A]
    a2 <- pop
  } yield a1 // + a2
  def mapStack[A, B](f: A => B): IndexedState[Stack[A], Stack[B], Unit] = for {
    s <- IndexedState.get[Stack[A]]
    _ <- IndexedState.put(s.map(f))
  } yield ()

  "IndexedState object" - {
    "pure" - {
      "returns IndexedState.Point" in {
        val pure = IndexedState.pure[String, Int](42)
        assert(pure.run("abc") === (42, "abc"))
      }
    }

    "point" - {
      "returns IndexedState.Point" in {
        val point = IndexedState.point[String, Int](42)
        assert(point.run("abc") === (42, "abc"))
      }
    }

    "get" - {
      "returns IndexedState.Get" in {
        val get = IndexedState.get[String]
        assert(get.run("abc") === ("abc", "abc"))
      }
    }

    "put" - {
      "returns IndexedState.Put" in {
        val put = IndexedState.put[Int, String]("abc")
        assert(put.run(42) === ((), "abc"))
      }
    }

    "modify" - {
      "returns IndexedState evaluating state" in {
        val modify = IndexedState.modify[Int, Double](_.toDouble)
        assert(modify.run(42) === ((), 42d))
      }
    }

    "apply" - {
      "returns IndexedState runs state" in {
        val apply = IndexedState.apply((i: Int) => (i.toDouble, i.toString))
        assert(apply.run(42) === (42d, "42"))
      }
    }
  }

  "IndexedState instance" - {
    "map" - {
      "creates new State using current value" in {
        val pure = IndexedState.pure[String, Int](42)

        val mapped = pure.map(_.toDouble)
        assert(mapped.run("abc") === (42d, "abc"))
      }
    }

    "flatMap" - {
      "runs Composed State" in {
        val get = IndexedState.get[String]
        val flatMap = get.flatMap { s =>
          IndexedState.put(s.toList.tails.map(list => String.valueOf(list.toArray)).toList)
        }

        assert(flatMap.run("abc") === ((), List("abc", "bc", "c", "")))
      }

      "returns new State" in {
        val state = for {
          _ <- push(42)
          _ <- mapStack[Int, Double](_.toDouble)
        } yield ()

        assert(state.exec(empty) === List(42d))
      }
    }
  }
}
