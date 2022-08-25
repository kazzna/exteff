package ext

import org.scalatest.freespec.AnyFreeSpec

import java.util.concurrent.Executors
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

class FreeSpec extends AnyFreeSpec {
  "Free" - {
    "extract" - {
      import ext.types.OptionMonad.monadInstance
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
      import ext.types.OptionMonad.monadInstance
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

    "flatMap" - {
      import ext.types.FutureMonad.monadInstance

      "should run in series" in {
        val list: ListBuffer[Int] = ListBuffer.empty

        def f(i: Int): () => Int = () => {
          Thread.sleep(i * 100)
          list.append(i)
          i
        }

        implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(8))

        val nt = new NaturalTransformation[() => *, Future] {
          override def apply[A](fa: () => A): Future[A] = Future(fa())
        }

        val free = for {
          a <- Free.lift(f(3))
          b <- Free.lift(f(2))
          c <- Free.lift(f(1))
        } yield List(a, b, c)

        val actual = Await.result(free.foldMap(nt), 3.seconds)
        assert(actual === List(3, 2, 1))
        assert(list.toList === List(3, 2, 1))
      }
    }

    "apply" - {
      import ext.types.FutureMonad.monadInstance
      "should run in parallel" in {
        val list: ListBuffer[Int] = ListBuffer.empty

        def f(i: Int): () => Int = () => {
          Thread.sleep(i * 100)
          list.append(i)
          i
        }

        implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(8))

        val nt = new NaturalTransformation[() => *, Future] {
          override def apply[A](fa: () => A): Future[A] = Future(fa())
        }

        val free1 = Free.lift(f(1)).map2(Free.lift(f(3)))((a, b) => List(a, b))
        val free2 = free1.map2(Free.lift(f(2)))((a, b) => b +: a)

        val actual = Await.result(free2.foldMap(nt), 3.seconds)
        assert(actual === List(2, 1, 3))
        assert(list.toList === List(1, 2, 3))
      }
    }
  }
}
