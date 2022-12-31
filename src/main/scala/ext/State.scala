package ext

import scala.annotation.tailrec

sealed abstract class State[S, A] {
  final def run(s: S): (A, S) = {
    @tailrec
    def loop(s: S, state: State[S, A]): (A, S) = state match {
      case primitive: State.Primitive[S, A] => primitive.runPrimitive(s)
      case step @ State.Step(_, _) =>
        step.state match {
          case primitive: State.Primitive[S, step.I] =>
            val (i, s2) = primitive.runPrimitive(s)
            loop(s2, step.arrows(i))
          case step2 @ State.Step(_, _) =>
            loop(s, State.Step(step2.state, step2.arrows.compose(step.arrows)))
        }
    }

    loop(s, this)
  }
  final def map[B](f: A => B): State[S, B] = this.ap(State.point(f))
  final def ap[B](f: State[S, A => B]): State[S, B] =
    this.flatMap(a => f.flatMap(f => State.point(f(a))))
  final def map2[B, C](state: State[S, B])(f: (A, B) => C): State[S, C] =
    map(f.curried).flatMap(f => state.map(b => f(b)))
  final def flatMap[B](f: A => State[S, B]): State[S, B] = this match {
    case State.Pure(a) => f(a)
    case State.Step(si, arrows) => State.Step(si, arrows.compose(State.Arrows(f)))
    case state => State.Step(state, State.Arrows(f))
  }
  final def eval(s: S): A = run(s)._1
  final def exec(s: S): S = run(s)._2
}

object State {
  def pure[S, A](a: A): State[S, A] = Pure(a)
  def apply[S, A](run: S => (A, S)): State[S, A] = for {
    s1 <- get[S]
    (a, s2) = run(s1)
    _ <- put(s2)
  } yield a
  @inline
  def point[S, A](a: A): State[S, A] = pure(a)
  def get[S]: State[S, S] = Get()
  def put[S](s: S): State[S, Unit] = Put(s)
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get[S]
    _ <- put(f(s))
  } yield ()

  sealed trait Primitive[S, A] extends State[S, A] {
    def runPrimitive(s: S): (A, S)
  }
  final case class Pure[S, A](a: A) extends Primitive[S, A] {
    override def runPrimitive(s: S): (A, S) = (a, s)
  }
  final case class Get[S] private () extends Primitive[S, S] {
    override def runPrimitive(s: S): (S, S) = (s, s)
  }
  final case class Put[S] private (s: S) extends Primitive[S, Unit] {
    override def runPrimitive(s: S): (Unit, S) = ((), this.s)
  }
  final case class Step[S, I0, A] private (_s: State[S, I0], _a: Arrows[S, I0, A]) extends State[S, A] {
    type I = I0
    def state: State[S, I] = _s
    def arrows: Arrows[S, I, A] = _a
  }

  sealed abstract class Arrows[S, A, B] {
    final def apply(a: A): State[S, B] = {
      @tailrec
      def loop[I](i: I, arrows: Arrows[S, I, B]): State[S, B] = arrows match {
        case Arrows.Single(f) => f(i)
        case composed @ Arrows.Composed(_, _) =>
          composed.arrows1 match {
            case Arrows.Single(f) => State.Step(f(i), composed.arrows2)
            case composed2 @ Arrows.Composed(_, _) =>
              loop(i, composed2.arrows1.compose(composed2.arrows2.compose(composed.arrows2)))
          }
      }

      loop(a, this)
    }

    final def compose[C](arrows: Arrows[S, B, C]): Arrows[S, A, C] =
      Arrows.Composed(this, arrows)
  }

  object Arrows {
    def apply[S, A, B](f: A => State[S, B]): Arrows[S, A, B] = Single(f)
    final case class Single[S, A, B] private (f: A => State[S, B]) extends Arrows[S, A, B]
    final case class Composed[S, A, B0, C] private (
      _a1: Arrows[S, A, B0],
      _a2: Arrows[S, B0, C]
    ) extends Arrows[S, A, C] {
      type B = B0
      def arrows1: Arrows[S, A, B] = _a1
      def arrows2: Arrows[S, B, C] = _a2
    }
  }
}
