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
    case arrow: Arrows.Arrow[F, A, B] => arrow.extractArrow(fa)
    case Arrows.SortedArrows(a1, a2) =>
      @tailrec
      def loop[C, D](fc: F[C], arrow: Arrows.Arrow[F, C, D], sorted: Arrows.Sorted[F, D, B]): F[B] = sorted match {
        case Arrows.SortedArrows(a1, a2) => loop(arrow.extractArrow(fc), a1, a2)
        case arrow2: Arrows.Arrow[F, D, B] => arrow2.extractArrow(arrow.extractArrow(fc))
      }

      loop(fa, a1, a2)
  }

  def extractApplicative(fa: F[A])(implicit F: Applicative[F]): Either[Eff[F, B], F[B]] = sorted match {
    case arrow: Arrows.Arrow[F, A, B] => arrow.extractArrowApplicative(fa)
    case Arrows.SortedArrows(a1, a2) =>
      @tailrec
      def loop[AA, C](
          efa: Either[Free[F, AA], F[AA]],
          a1: Arrows.Arrow[F, AA, C],
          a2: Arrows.Sorted[F, C, B]
      ): Either[Free[F, B], F[B]] = efa match {
        case Left(free) => Left(free.bindArrows(a1.andThen(a2)))
        case Right(fa) =>
          a2 match {
            case a3: Arrows.Arrow[F, C, B] =>
              a1.extractArrowApplicative(fa) match {
                case Left(freeC) => Left(freeC.bindArrows(a3))
                case Right(fc) => a3.extractArrowApplicative(fc)
              }
            case Arrows.SortedArrows(a21, a22) => loop(a1.extractArrowApplicative(fa), a21, a22)
          }
      }

      loop(Right(fa), a1, a2)
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

  private final case class Junction[F[_], A, B, C](
    a1: Arrows[F, A, B],
    a2: Arrows[F, B, C]
  ) extends Arrows[F, A, C]

  private sealed trait Sorted[F[_], A, B] extends Arrows[F, A, B]
  private final case class SortedArrows[F[_], A, B, C](
    a1: Arrow[F, A, B],
    a2: Sorted[F, B, C]
  ) extends Sorted[F, A, C]

  private sealed trait Arrow[F[_], A, B] extends Sorted[F, A, B] {
    final def extractArrow(fa: F[A])(implicit F: Monad[F]): F[B] = this match {
      case Point() => fa.asInstanceOf[F[B]]
      case Apply(f) => F.ap(fa)(f.extract)
      case Bind(f, nt) => F.bind(fa)(a => f(a).transform(nt).extract)
    }

    final def extractArrowApplicative(fa: F[A])(implicit F: Applicative[F]): Either[Free[F, B], F[B]] = this match {
      case Point() => Right(fa.asInstanceOf[F[B]])
      case Apply(f) => eitherApplicative.ap(Right(fa))(f.extractApplicative)
      case bind @ Bind(_, _) => Left(Free.impure(fa, bind))
    }
  }

  private type EA[F[_]] = { type R[A] = Either[Free[F, A], F[A]] }

  private def eitherApplicative[F[_]](implicit F: Applicative[F]): Applicative[EA[F]#R] = new Applicative[EA[F]#R] {
    override def point[A](a: A): Either[Free[F, A], F[A]] = Right(F.point(a))

    override def map2[A, B, C](
        fa: Either[Free[F, A], F[A]]
    )(
        fb: Either[Free[F, B], F[B]]
    )(
        f: (A, B) => C
    ): Either[Free[F, C], F[C]] = (fa, fb) match {
      case (Left(freeA), Left(freeB)) => Left(freeA.map2(freeB)(f))
      case (Left(freeA), Right(fb)) => Left(freeA.ap(Free.lift(F.map(fb)(b => (a: A) => f(a, b)))))
      case (Right(fa), Left(freeB)) => Left(freeB.ap(Free.lift(F.map(fa)(f.curried))))
      case (Right(fa), Right(fb)) => Right(F.map2(fa)(fb)(f))
    }

    override def ap[A, B](
        fa: Either[Free[F, A], F[A]]
    )(f: Either[Free[F, A => B], F[A => B]]): Either[Free[F, B], F[B]] = (fa, f) match {
      case (Left(freeA), Left(freeF)) => Left(freeA.ap(freeF))
      case (Left(freeA), Right(ff)) => Left(freeA.ap(Free.lift(ff)))
      case (Right(fa), Left(freeF)) => Left(Free.lift(fa).ap(freeF))
      case (Right(fa), Right(ff)) => Right(F.ap(fa)(ff))
    }
  }

  private final case class Point[F[_], A]() extends Arrow[F, A, A]
  private final case class Apply[F[_], A, B](f: Free[F, A => B]) extends Arrow[F, A, B]
  private final case class Bind[F[_], G[_], A, B](f: A => Free[F, B], nt: NaturalTransformation[F, G])
      extends Arrow[G, A, B]
}
