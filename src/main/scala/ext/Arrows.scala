package ext

import ext.types.{Applicative, Monad}

import scala.annotation.tailrec

sealed trait Arrows[F[_], A, B] {
  final def thenApply[C](f: Free[F, B => C]): Arrows[F, A, C] = andThen(Arrows.apply(f))
  final def thenBind[C](f: B => Free[F, C]): Arrows[F, A, C] = andThen(Arrows.bind(f))

  final def andThen[C](arrows: Arrows[F, B, C]): Arrows[F, A, C] = (this, arrows) match {
    case (Arrows.Point(), _) => arrows.asInstanceOf[Arrows[F, A, C]]
    case (_, Arrows.Point()) => this.asInstanceOf[Arrows[F, A, C]]
    case _ => Arrows.junction(this, arrows)
  }

  final def transform[G[_]](nt: NaturalTransformation[F, G]): Arrows[G, A, B] = {
    @tailrec
    def loop[C](a1: Arrows[G, A, C], a2: Arrows.Sorted[F, C, B]): Arrows[G, A, B] = a2 match {
      case Arrows.SortedArrows(arrow, a2b) =>
        loop(a1.andThen(Arrows.transformArrow(arrow, nt)), a2b)
      case arrow: Arrows.Arrow[F, C, B] =>
        a1.andThen(Arrows.transformArrow(arrow, nt))
    }

    loop(Arrows.point, sorted)
  }

  private def sorted: Arrows.Sorted[F, A, B] = this match {
    case Arrows.Junction(a1, a2) =>
      @tailrec
      def loop[C](a1: Arrows[F, A, C], a2: Arrows[F, C, B]): Arrows.Sorted[F, A, B] = a2 match {
        case Arrows.Junction(a2a, a2b) => loop(a1.andThen(a2a), a2b)
        case sorted: Arrows.Sorted[F, C, B] =>
          a1 match {
            case Arrows.Junction(a1a, a1b) =>
              a1b match {
                case Arrows.Junction(a1b1, a1b2) => loop(a1a.andThen(a1b1), a1b2.andThen(a2))
                case Arrows.SortedArrows(a1b1, a1b2) => loop(a1a.andThen(a1b1), a1b2.andThen(a2))
                case arrow: Arrows.Arrow[F, _, C] => loop(a1a, Arrows.sorted(arrow, sorted))
                case _ => sys.error("a1b should not be other than these.")
              }
            case Arrows.SortedArrows(a1a, a1b) =>
              a1b match {
                case Arrows.SortedArrows(a1b1, a1b2) => loop(a1a.andThen(a1b1), a1b2.andThen(a2))
                case arrow: Arrows.Arrow[F, _, C] => loop(a1a, Arrows.sorted(arrow, sorted))
              }
            case arrow: Arrows.Arrow[F, A, C] => Arrows.sorted(arrow, sorted)
          }
      }

      loop(a1, a2)
    case sorted @ Arrows.SortedArrows(_, _) => sorted
    case arrow: Arrows.Arrow[F, A, B] => arrow
  }

  def resume(a: A): Free[F, B] = sorted match {
    case arrow: Arrows.Arrow[F, A, B] => Arrows.applyArrow(arrow, a)
    case Arrows.SortedArrows(a1, a2) => Arrows.applyArrow(a1, a).bindArrows(a2)
  }

  def extract(fa: F[A])(implicit F: Monad[F]): F[B] = sorted match {
    case arrow: Arrows.Arrow[F, A, B] => Arrows.extractArrow(arrow, fa)
    case Arrows.SortedArrows(a1, a2) =>
      @tailrec
      def loop[C, D](fc: F[C], arrow: Arrows.Arrow[F, C, D], sorted: Arrows.Sorted[F, D, B]): F[B] = sorted match {
        case Arrows.SortedArrows(a1, a2) => loop(Arrows.extractArrow(arrow, fc), a1, a2)
        case arrow2: Arrows.Arrow[F, D, B] => Arrows.extractArrow(arrow2, Arrows.extractArrow(arrow, fc))
      }

      loop(fa, a1, a2)
  }
}

object Arrows {
  def point[F[_], A]: Arrows[F, A, A] = Point()
  def apply[F[_], A, B](f: Free[F, A => B]): Arrows[F, A, B] = Apply(f)
  def bind[F[_], A, B](f: A => Free[F, B]): Arrows[F, A, B] = Bind(f, NaturalTransformation.reflect)

  private def junction[F[_], A, B, C](arrows1: Arrows[F, A, B], arrows2: Arrows[F, B, C]): Arrows[F, A, C] =
    Junction(arrows1, arrows2)

  private def sorted[F[_], A, B, C](arrow: Arrow[F, A, B], sorted: Sorted[F, B, C]): Sorted[F, A, C] =
    SortedArrows(arrow, sorted)

  private def transformArrow[F[_], S[_], A, B](
      arrow: Arrow[F, A, B],
      nt: NaturalTransformation[F, S]
  ): Arrow[S, A, B] = arrow match {
    case Point() => Point().asInstanceOf[Arrows.Arrow[S, A, B]]
    case Apply(f) => Apply(f.transform(nt))
    case Bind(f, nt0) => Bind(f, nt0.compose(nt))
  }

  private def applyArrow[F[_], A, B](arrow: Arrow[F, A, B], a: A): Free[F, B] = arrow match {
    case Point() => Free.point(a).asInstanceOf[Free[F, B]]
    case Apply(f) => f.map(f => f(a))
    case Bind(f, nt) => f(a).transform(nt)
  }

  private def extractArrow[F[_], A, B](arrow: Arrow[F, A, B], fa: F[A])(implicit F: Monad[F]): F[B] = arrow match {
    case Point() => fa.asInstanceOf[F[B]]
    case Apply(f) => F.apply(fa)(f.extract)
    case Bind(f, nt) => F.bind(fa)(a => f(a).transform(nt).extract)
  }

  private final case class Junction[F[_], A, B, C](
    a1: Arrows[F, A, B],
    a2: Arrows[F, B, C]
  ) extends Arrows[F, A, C]

  private sealed trait Sorted[F[_], A, B] extends Arrows[F, A, B]
  private final case class SortedArrows[F[_], A, B, C](
    a1: Arrow[F, A, B],
    a2: Sorted[F, B, C]
  ) extends Sorted[F, A, C]

  private sealed trait Arrow[F[_], A, B] extends Sorted[F, A, B]

  private final case class Point[F[_], A]() extends Arrow[F, A, A]
  private final case class Apply[F[_], A, B](f: Free[F, A => B]) extends Arrow[F, A, B]
  private final case class Bind[F[_], G[_], A, B](f: A => Free[F, B], nt: NaturalTransformation[F, G])
      extends Arrow[G, A, B]
}
