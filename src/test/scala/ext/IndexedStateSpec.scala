package ext

import org.scalatest.freespec.AnyFreeSpec

class IndexedStateSpec extends AnyFreeSpec {
  type Stack[A] = List[A]
  type NonEmptyStack[A] = List[A]
  type SwappableStack[A] = List[A]

  def empty[A]: Stack[A] = List.empty

  def pushStack[A](a: A): IndexedState[Stack[A], NonEmptyStack[A], Unit] = ???
  def pushNonEmpty[A](a: A): IndexedState[Stack[A], NonEmptyStack[A], Unit] = ???

  def popSwappable[A]: IndexedState[SwappableStack[A], NonEmptyStack[A], A] = ???
  def popNonEmpty[A]: IndexedState[NonEmptyStack[A], Stack[A], A] = ???

  def swap[A]: IndexedState[SwappableStack[A], SwappableStack[A], Unit] = ???

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
    }
  }
}
