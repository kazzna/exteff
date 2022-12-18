package ext

import scala.annotation.tailrec

sealed abstract class State[S, A] {
  def run(s: S): (A, S)
  final def map[B](f: A => B): State[S, B] = this.ap(State.point(f))
  final def ap[B](f: State[S, A => B]): State[S, B] =
    this.flatMap(a => f.flatMap(f => State.point(f(a))))
  final def map2[B, C](state: State[S, B])(f: (A, B) => C): State[S, C] =
    map(f.curried).flatMap(f => state.map(b => f(b)))
  final def flatMap[B](f: A => State[S, B]): State[S, B] = this match {
    case State.Pure(a) => f(a)
    case State.FlatMap(si, arrows) => State.FlatMap(si, arrows.compose(State.Arrows(f)))
    case state => State.FlatMap(state, State.Arrows(f))
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
    override final def run(s: S): (A, S) = runPrimitive(s)
    protected def runPrimitive(s: S): (A, S)
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
  final case class FlatMap[S, I, A] private (si: State[S, I], arrows: Arrows[S, I, A]) extends State[S, A] {
    override def run(s: S): (A, S) = {
      @tailrec
      def loop[J](s: S, sj: State[S, J], arrows: Arrows[S, J, A]): (A, S) = sj match {
        case state: Primitive[S, J] =>
          val (j, s2) = state.run(s)
          arrows(j) match {
            case state: Primitive[S, A] => state.run(s2)
            case FlatMap(sk, arrows) => loop(s2, sk, arrows)
          }
        case FlatMap(sk, arrows0) => loop(s, sk, arrows0.compose(arrows))
      }

      loop(s, si, arrows)
    }
  }

  sealed abstract class Arrows[S, A, B] {
    final def apply(a: A): State[S, B] = sorted match {
      case Left(Arrows.Single(f)) => f(a)
      case Right(Arrows.Sorted(Arrows.Single(f), tail)) => State.FlatMap(f(a), tail)
    }

    final def compose[C](arrows: Arrows[S, B, C]): Arrows[S, A, C] =
      Arrows.Composed(this, arrows)

    private final def sorted: Either[Arrows.Single[S, A, B], Arrows.Sorted[S, A, _, B]] = {
      @tailrec
      def loop[I](a1: Arrows[S, A, I], a2: Arrows[S, I, B]): Arrows.Sorted[S, A, _, B] = a2 match {
        case Arrows.Composed(a21, a22) => loop(a1.compose(a21), a22)
        case a2 =>
          a1 match {
            case a1 @ Arrows.Single(_) => Arrows.Sorted(a1, a2)
            case Arrows.Composed(a11, a12) =>
              a12 match {
                case a12 @ Arrows.Single(_) => loop(a11, Arrows.Sorted(a12, a2))
                case Arrows.Composed(a121, a122) => loop(a11.compose(a121).compose(a122), a2)
                case Arrows.Sorted(a121, a122) => loop(a11.compose(a121).compose(a122), a2)
              }
            case Arrows.Sorted(a11, a12) => loop(a11.compose(a12), a2)
          }
      }

      this match {
        case single @ Arrows.Single(_) => Left(single)
        case Arrows.Composed(arrows1, arrows2) => Right(loop(arrows1, arrows2))
        case sorted @ Arrows.Sorted(_, _) => Right(sorted)
      }
    }
  }

  object Arrows {
    def apply[S, A, B](f: A => State[S, B]): Arrows[S, A, B] = Single(f)
    final case class Single[S, A, B] private (f: A => State[S, B]) extends Arrows[S, A, B]
    final case class Composed[S, A, B, C] private (
      arrows1: Arrows[S, A, B],
      arrows2: Arrows[S, B, C]
    ) extends Arrows[S, A, C]
    final case class Sorted[S, A, B, C] private (
      head: Single[S, A, B],
      tail: Arrows[S, B, C]
    ) extends Arrows[S, A, C]
  }
}
