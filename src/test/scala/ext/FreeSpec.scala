package ext

import org.scalatest.freespec.AnyFreeSpec

import java.util.concurrent.Executors
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

class FreeSpec extends AnyFreeSpec {
  "Free" - {
    "extract" - {
      "pure" in {
        import types.OptionMonad.monadInstance

        val free: Free[Option, Int] = Free.pure(42)
        assert(free.extract === Option(42))
      }

      "pure mapped" - {
        import types.OptionMonad.monadInstance

        val free: Free[Option, Int] = Free.pure(42)
        assert(free.map(_.toString).extract === Option("42"))
      }

      "lift" in {
        import types.OptionMonad.monadInstance
        val option = Option(42)

        val free = Free.lift(option)
        assert(free.extract === Option(42))
      }

      "lift mapped" in {
        import types.OptionMonad.monadInstance

        val free = Free.lift(Option(42)).map(_.toString)
        assert(free.extract === Option("42"))
      }
    }

    "foldMap" - {
      case class Box[A](a: A)

      "to Option" in {
        import types.OptionMonad.monadInstance
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

    "composed" - {
      def fixed(numberOfThreads: Int): ExecutionContext =
        ExecutionContext.fromExecutor(Executors.newFixedThreadPool(numberOfThreads))

      def funcToFuture()(implicit ec: ExecutionContext): NaturalTransformation[() => *, Future] =
        new NaturalTransformation[() => *, Future] {
          override def apply[A](fa: () => A): Future[A] = Future(fa())
        }

      def createFunc(listBuffer: ListBuffer[Int]): Int => () => Int = i =>
        () => {
          Thread.sleep(i * 100)
          listBuffer.append(i)
          i
        }

      "monadic" - {
        "should run in series" in {
          val list: ListBuffer[Int] = ListBuffer.empty
          val f = createFunc(list)

          val free = for {
            a <- Free.lift(f(3))
            b <- Free.lift(f(2))
            c <- Free.lift(f(1))
          } yield List(a, b, c)

          implicit val ec: ExecutionContext = fixed(8)
          import types.FutureMonad.monadInstance
          val nt = funcToFuture()
          val actual = Await.result(free.foldMap(nt), 3.seconds)

          assert(actual === List(3, 2, 1))
          assert(list.toList === List(3, 2, 1))
        }

        "should not be extracted by extractApplicative" in {
          val list: ListBuffer[Int] = ListBuffer.empty
          val f = createFunc(list)

          val free2 = Free.lift(f(2))
          val free3 = Free.lift(f(3))

          val free23 = free2.map2(free3)((a, b) => List(a, b))
          val free231 = for {
            as <- free23
            a <- Free.lift(f(1))
          } yield a +: as

          implicit val ec: ExecutionContext = fixed(8)
          import types.FutureMonad.applicativeInstance
          val free = free231.transform(funcToFuture())
          val actual = free.extractApplicative

          assert(actual.isLeft)
          Thread.sleep(400)
          assert(list.toList === List(2, 3))
        }
      }

      "applicative" - {
        "should run in parallel" in {
          val list: ListBuffer[Int] = ListBuffer.empty
          val f = createFunc(list)

          val free1 = Free.lift(f(1)).map2(Free.lift(f(3)))((a, b) => List(a, b))
          val free2 = free1.map2(Free.lift(f(2)))((a, b) => b +: a)

          implicit val ec: ExecutionContext = fixed(8)
          import types.FutureMonad.monadInstance
          val nt = funcToFuture()
          val actual = Await.result(free2.foldMap(nt), 3.seconds)

          assert(actual === List(2, 1, 3))
          assert(list.toList === List(1, 2, 3))
        }
      }

      "should be extracted by extractApplicative" in {
        val list: ListBuffer[Int] = ListBuffer.empty
        val f = createFunc(list)

        val free1 = Free.lift(f(1))
        val free2 = Free.lift(f(2))
        val free3 = Free.lift(f(3))

        val free23 = free2.map2(free3)((a, b) => List(a, b))
        val free123 = free23.map2(free1)((a, b) => b +: a)

        implicit val ec: ExecutionContext = fixed(8)
        import types.FutureMonad.monadInstance
        val free = free123.transform(funcToFuture())
        val either = free.extractApplicative

        assert(either.isRight)

        val actual = either.map(s => Await.result(s, 3.seconds)).left.map(_ => List.empty[Int]).merge

        assert(actual === List(1, 2, 3))
        assert(list.toList === List(1, 2, 3))
      }
    }
  }
}
