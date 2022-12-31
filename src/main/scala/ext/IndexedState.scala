package ext

import scala.annotation.tailrec

sealed abstract class IndexedState[S, T, A] {
  final def run(s: S): (A, T) = {
    @tailrec
    def loop[U](u: U, state: IndexedState[U, T, A]): (A, T) = state match {
      case primitive: IndexedState.Primitive[U, T, A] => primitive.runPrimitive(u)
      case step @ IndexedState.FlatMap(_, _) =>
        step.state match {
          case primitive: IndexedState.Primitive[U, step.T, step.I] =>
            val (i, v) = primitive.runPrimitive(u)
            loop(v, step.arrows(i))
          case step2 @ IndexedState.FlatMap(_, _) =>
            loop(u, IndexedState.FlatMap(step2.state, step2.arrows.compose(step.arrows)))
        }
    }

    loop(s, this)
  }
  final def map[B](f: A => B): IndexedState[S, T, B] = flatMap(a => IndexedState.pure(f(a)))
  final def flatMap[U, B](f: A => IndexedState[T, U, B]): IndexedState[S, U, B] =
    IndexedState.FlatMap[S, T, U, A, B](this, IndexedState.Arrows.Single(f))
  final def exec(s: S): T = run(s)._2
}

object IndexedState {
  @inline
  def pure[S, A](a: A): IndexedState[S, S, A] = point(a)
  def point[S, A](a: A): IndexedState[S, S, A] = Point(a)
  def get[S]: IndexedState[S, S, S] = Get()
  def put[S, T](t: T): IndexedState[S, T, Unit] = Put(t)
  def modify[S, T](eval: S => T): IndexedState[S, T, Unit] = for {
    s <- get[S]
    _ <- put(eval(s))
  } yield ()
  def apply[S, T, A](run: S => (A, T)): IndexedState[S, T, A] = for {
    s <- get[S]
    (a, t) = run(s)
    _ <- put(t)
  } yield a

  sealed abstract class Primitive[S, T, A] extends IndexedState[S, T, A] {
    def runPrimitive(s: S): (A, T)
  }
  final case class Point[S, A] private (a: A) extends Primitive[S, S, A] {
    override def runPrimitive(s: S): (A, S) = (a, s)
  }
  final case class Get[S] private () extends Primitive[S, S, S] {
    override def runPrimitive(s: S): (S, S) = (s, s)
  }
  final case class Put[S, T] private (t: T) extends Primitive[S, T, Unit] {
    override def runPrimitive(s: S): (Unit, T) = ((), t)
  }
  final case class FlatMap[S, T0, U, I0, A](
    _s: IndexedState[S, T0, I0],
    _a: Arrows[T0, U, I0, A]
  ) extends IndexedState[S, U, A] {
    type T = T0
    type I = I0
    def state: IndexedState[S, T, I] = _s
    def arrows: Arrows[T, U, I, A] = _a
  }

  sealed abstract class Arrows[T, U, A, B] {
    def apply(a: A): IndexedState[T, U, B] = {
      @tailrec
      def loop(arrows: Arrows[T, U, A, B]): IndexedState[T, U, B] = arrows match {
        case Arrows.Single(f) => f(a)
        case composed @ Arrows.Composed(_, _) =>
          composed.arrows1 match {
            case Arrows.Single(f) => f(a) match {
              case primitive: IndexedState.Primitive[T, composed.U, composed.B] =>
                IndexedState.FlatMap(primitive, composed.arrows2)
              case step @ IndexedState.FlatMap(_ ,_) =>
                IndexedState.FlatMap(step.state, step.arrows.compose(composed.arrows2))
            }
            case composed2 @ Arrows.Composed(_, _) =>
              loop(composed2.arrows1.compose(composed2.arrows2.compose(composed.arrows2)))
          }
      }

      loop(this)
    }
    def compose[V, C](arrows: Arrows[U, V, B, C]): Arrows[T, V, A, C] = Arrows.Composed(this, arrows)
  }

  object Arrows {
    final case class Single[T, U, A, B] private (f: A => IndexedState[T, U, B]) extends Arrows[T, U, A, B]
    final case class Composed[T, U0, V, A, B0, C] private (
      _a1: Arrows[T, U0, A, B0],
      _a2: Arrows[U0, V, B0, C]
    ) extends Arrows[T, V, A, C] {
      type U = U0
      type B = B0
      def arrows1: Arrows[T, U, A, B] = _a1
      def arrows2: Arrows[U, V, B, C] = _a2
    }
  }
}
