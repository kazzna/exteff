package ext

import types.{Applicative, Functor, Monad}

import scala.annotation.tailrec

sealed abstract class Free[F[_], A] {
  final def map[B](f: A => B): Free[F, B] = bind(Free.Arrows.map(f))
  final def ap[B](free: Free[F, A => B]): Free[F, B] = bind(Free.Arrows.apply(free))
  final def map2[B, C](free: Free[F, B])(f: (A, B) => C): Free[F, C] =
    ap(Free.point[F, A => B => C](f.curried)).ap(free.map(b => (f: B => C) => f(b)))

  final def flatMap[B](f: A => Free[F, B]): Free[F, B] = bind(Free.Arrows.bind(f))
  final def transform[G[_]](nt: NaturalTransformation[F, G]): Free[G, A] = this match {
    case Free.Pure(a) => Free.pure(a)
    case Free.Lifted(fa) => Free.lift(nt(fa))
    case impure @ Free.Impure(_, _) => Free.lift(nt(impure.fi)).bind(impure.arrows.transform(nt))
  }
  final def extract(implicit F: Monad[F]): F[A] = this match {
    case Free.Pure(a) => F.point(a)
    case Free.Lifted(fa) => fa
    case impure @ Free.Impure(_, _) => impure.arrows.run(impure.fi)
  }
  final def foldMap[G[_]: Monad](nt: NaturalTransformation[F, G]): G[A] = transform(nt).extract

  @inline
  private[ext] final def bind[B](arrows: Free.Arrows[F, A, B]): Free[F, B] = this match {
    case Free.Pure(a) => arrows(a)
    case Free.Lifted(fa) => Free.impure(fa, arrows)
    case impure @ Free.Impure(_, _) => Free.impure(impure.fi, impure.arrows.compose(arrows))
  }
}

object Free {
  def pure[F[_], A](a: A): Free[F, A] = Pure(a)
  @inline
  def point[F[_], A](a: A): Free[F, A] = pure(a)
  def lift[F[_], A](fa: F[A]): Free[F, A] = Lifted(fa)
  private def impure[F[_], I, A](fi: F[I], arrows: Arrows[F, I, A]): Free[F, A] = Impure(fi, arrows)

  implicit final class Flatten[F[_], A](val free: Free[F, Free[F, A]]) extends AnyVal {
    def flatten: Free[F, A] = free.flatMap(identity)
  }

  final case class Pure[F[_], A] private (a: A) extends Free[F, A]
  final case class Lifted[F[_], A] private (fa: F[A]) extends Free[F, A]
  final case class Impure[F[_], I0, A] private (
    _fi: F[I0],
    _arrows: Arrows[F, I0, A]
  ) extends Free[F, A] {
    type I = I0
    def fi: F[I] = _fi
    def arrows: Arrows[F, I, A] = _arrows
  }

  sealed abstract class Arrows[F[_], A, B] {
    final def compose[C](arrows: Arrows[F, B, C]): Arrows[F, A, C] = Arrows.composed(this, arrows)

    final def apply(a: A): Free[F, B] = {
      @tailrec
      def loop(arrows: Arrows[F, A, B]): Free[F, B] = arrows match {
        case single @ Arrows.Single(_, _) => single.arrow.transform(single.nt)(a)
        case composed @ Arrows.Composed(_, _, _) =>
          val arrows1 = composed.arrows1.transform(composed.nt)
          val arrows2 = composed.arrows2.transform(composed.nt)
          arrows1 match {
            case single @ Arrows.Single(_, _) => single.arrow.transform(single.nt)(a).bind(arrows2)
            case composed @ Arrows.Composed(_, _, _) =>
              val arrows1a = composed.arrows1.transform(composed.nt)
              val arrows1b = composed.arrows2.transform(composed.nt)
              loop(arrows1a.compose(arrows1b.compose(arrows2)))
          }
      }

      loop(this)
    }

    final def transform[G[_]](nt: NaturalTransformation[F, G]): Arrows[G, A, B] = this match {
      case single @ Arrows.Single(_, _) => Arrows.Single(single.arrow, single.nt.compose(nt))
      case composed @ Arrows.Composed(_, _, _) =>
        Arrows.Composed(composed.arrows1, composed.arrows2, composed.nt.compose(nt))
    }

    final def resume(fa: F[A])(implicit F: Applicative[F]): Either[Either[F[Free[F, B]], Free[F, B]], F[B]] = {
      @tailrec
      def loop[I](fi: F[I], arrows: Arrows[F, I, B]): Either[Either[F[Free[F, B]], Free[F, B]], F[B]] = arrows match {
        case single @ Arrows.Single(_, _) =>
          single.arrow.transform(single.nt) match {
            case Arrows.Arrow.Map(f) => Right(F.map(fi)(f))
            case Arrows.Arrow.Lift(f) => Right(F.ap(fi)(f))
            case apply @ Arrows.Arrow.Apply(_, _) => Left(Right(apply.resumeApply(fi)))
            case bind @ Arrows.Arrow.Bind(_, _) => Left(Left(F.map(fi)(bind.f.andThen(_.transform(bind.nt)))))
          }
        case composed @ Arrows.Composed(_, _, _) =>
          val arrows1 = composed.arrows1.transform(composed.nt)
          val arrows2 = composed.arrows2.transform(composed.nt)
          arrows1 match {
            case single @ Arrows.Single(_, _) =>
              single.arrow.transform(single.nt) match {
                case Arrows.Arrow.Map(f) => Left(Right(Free.lift(F.map(fi)(f)).bind(arrows2)))
                case Arrows.Arrow.Lift(f) => Left(Right(Free.lift(F.ap(fi)(f)).bind(arrows2)))
                case apply @ Arrows.Arrow.Apply(_, _) => Left(Right(apply.resumeApply(fi).bind(arrows2)))
                case bind @ Arrows.Arrow.Bind(_, _) =>
                  Left(Left(F.map(fi)(bind.f.andThen(_.transform(bind.nt).bind(arrows2)))))
              }
            case composed @ Arrows.Composed(_, _, _) =>
              val arrows1a = composed.arrows1.transform(composed.nt)
              val arrows1b = composed.arrows2.transform(composed.nt)
              loop(fi, arrows1a.compose(arrows1b.compose(arrows2)))
          }
      }

      loop(fa, this)
    }

    final def run(fa: F[A])(implicit F: Monad[F]): F[B] = {
      def runResume(free: Free[F, B]): F[Free[F, B]] = free match {
        case pure @ Free.Pure(_) => F.point[Free[F, B]](pure)
        case Free.Lifted(fa) => F.map(fa)(Free.pure[F, B])
        case impure @ Free.Impure(_, _) =>
          impure.arrows.resume(impure.fi) match {
            case Left(either) =>
              either match {
                case Left(ff) => ff
                case Right(free) => F.point(free)
              }
            case Right(fb) => F.point(Free.lift(fb))
          }
      }

      def finalize(free: Free[F, B]): F[B] = free match {
        case Free.Pure(a) => F.point(a)
        case Free.Lifted(fa) => fa
        case impure @ Free.Impure(_, _) => impure.fi.asInstanceOf[F[B]]
      }

      @tailrec
      def loopF(fFree: F[Free[F, B]]): F[B] = {
        val resumed = F.bind(fFree)(runResume)

        var done = true
        F.map(resumed) {
          case Free.Pure(_) => ()
          case Free.Lifted(_) => ()
          case _ => done = false
        }

        if (done) F.bind(resumed)(finalize)
        else loopF(resumed)
      }

      @tailrec
      def loop[C](fa: F[C], arrows: Arrows[F, C, B]): F[B] = arrows.resume(fa) match {
        case Left(either) =>
          either match {
            case Left(ff) => loopF(ff)
            case Right(free) =>
              free match {
                case Free.Pure(a) => F.point(a)
                case Free.Lifted(fa) => fa
                case impure @ Free.Impure(_, _) => loop(impure.fi, impure.arrows)
              }
          }
        case Right(fb) => fb
      }

      loop(fa, this)
    }

    private final def tryToFree: Option[Free[F, A => B]] = {
      def compose[I, J, K](free: Free[F, I => J], arrow: Arrows.Arrow[F, J, K]): Option[Free[F, I => K]] =
        arrow.tryToFree.map(free2 => free.map2(free2)(_.andThen(_)))

      @tailrec
      def loop[I](
          arrows: Arrows[F, I, B],
          acc: Option[Free[F, A => I]]
      ): Option[Free[F, A => B]] = acc match {
        case None => None
        case Some(free1) =>
          arrows match {
            case single @ Arrows.Single(_, _) =>
              compose(free1, single.arrow.transform(single.nt))
            case composed @ Arrows.Composed(_, _, _) =>
              val arrows1 = composed.arrows1.transform(composed.nt)
              val arrows2 = composed.arrows2.transform(composed.nt)
              arrows1 match {
                case single @ Arrows.Single(_, _) =>
                  loop(arrows2, compose(free1, single.arrow.transform(single.nt)))
                case composed @ Arrows.Composed(_, _, _) =>
                  val arrows1a = composed.arrows1.transform(composed.nt)
                  val arrows1b = composed.arrows2.transform(composed.nt)
                  loop(arrows1a.compose(arrows1b.compose(arrows2)), acc)
              }
          }
      }

      loop(this, Some(Free.pure[F, A => A](identity)))
    }
  }

  object Arrows {
    def map[F[_], A, B](f: A => B): Arrows[F, A, B] = single(Arrow.map(f))
    def apply[F[_], A, B](free: Free[F, A => B]): Arrows[F, A, B] = free match {
      case Free.Pure(f) => single(Arrow.map(f))
      case Free.Lifted(f) => single(Arrow.lift(f))
      case impure @ Free.Impure(_, _) => single(Arrow.apply(impure.fi, impure.arrows))
    }
    def bind[F[_], A, B](f: A => Free[F, B]): Arrows[F, A, B] = single(Arrow.bind(f))
    def lift[F[_], A, B](f: F[A => B]): Arrows[F, A, B] = single(Arrow.lift(f))
    private def composed[F[_], A, B, C](arrows1: Arrows[F, A, B], arrows2: Arrows[F, B, C]): Arrows[F, A, C] =
      Composed(arrows1, arrows2, NaturalTransformation.reflect)

    private def single[F[_], A, B](arrow: Arrow[F, A, B]): Arrows[F, A, B] =
      Single(arrow, NaturalTransformation.reflect)

    private implicit class Interleave[F[_], A, B, C](val arrows: Arrows[F, A, B => C]) extends AnyVal {
      def interleave(fb: F[B])(implicit F: Functor[F]): Arrows[F, A, C] =
        arrows.compose(Arrows.lift(F.map(fb)(b => (f: B => C) => f(b))))
    }

    final case class Single[F0[_], G[_], A, B] private (
      _arrow: Arrow[F0, A, B],
      _nt: NaturalTransformation[F0, G]
    ) extends Arrows[G, A, B] {
      type F[C] = F0[C]
      def arrow: Arrow[F, A, B] = _arrow
      def nt: NaturalTransformation[F, G] = _nt
    }
    final case class Composed[F0[_], G[_], A, B0, C] private (
      _arrows1: Arrows[F0, A, B0],
      _arrows2: Arrows[F0, B0, C],
      _nt: NaturalTransformation[F0, G]
    ) extends Arrows[G, A, C] {
      type F[D] = F0[D]
      type B = B0
      def arrows1: Arrows[F, A, B] = _arrows1
      def arrows2: Arrows[F, B, C] = _arrows2
      def nt: NaturalTransformation[F, G] = _nt
    }

    sealed abstract class Arrow[F[_], A, B] {
      final def apply(a: A): Free[F, B] = this match {
        case Arrow.Map(f) => Free.point(f(a))
        case Arrow.Lift(f) => Free.lift(f).map(_(a))
        case apply @ Arrow.Apply(_, _) => Free.lift(apply.fi).bind(apply.arrows).map(f => f(a))
        case bind @ Arrow.Bind(_, _) => bind.f(a).transform(bind.nt)
      }

      final def transform[G[_]](nt: NaturalTransformation[F, G]): Arrow[G, A, B] = this match {
        case Arrow.Map(f) => Arrow.map(f)
        case Arrow.Lift(f) => Arrow.lift(nt(f))
        case apply @ Arrow.Apply(_, _) => Arrow.Apply(nt(apply.fi), apply.arrows.transform(nt))
        case bind @ Arrow.Bind(_, _) => Arrow.Bind(bind.f, bind.nt.compose(nt))
      }

      final def tryToFree: Option[Free[F, A => B]] = this match {
        case Arrows.Arrow.Map(f) => Some(Free.point(f))
        case Arrows.Arrow.Lift(f) => Some(Free.lift(f))
        case apply @ Arrows.Arrow.Apply(_, _) => Some(Free.impure(apply.fi, apply.arrows))
        case Arrows.Arrow.Bind(_, _) => None
      }
    }

    object Arrow {
      def map[F[_], A, B](f: A => B): Arrow[F, A, B] = Map(f)
      def lift[F[_], A, B](f: F[A => B]): Arrow[F, A, B] = Lift(f)
      def apply[F[_], I, A, B](fi: F[I], arrows: Arrows[F, I, A => B]): Arrow[F, A, B] = Apply(fi, arrows)
      def bind[F[_], A, B](f: A => Free[F, B]): Arrow[F, A, B] = Bind(f, NaturalTransformation.reflect)

      final case class Map[F[_], A, B] private (f: A => B) extends Arrow[F, A, B]
      final case class Lift[F[_], A, B] private (f: F[A => B]) extends Arrow[F, A, B]
      final case class Apply[F[_], I0, A, B] private (
        _fi: F[I0],
        _arrows: Arrows[F, I0, A => B]
      ) extends Arrow[F, A, B] {
        type I = I0
        def fi: F[I] = _fi
        def arrows: Arrows[F, I, A => B] = _arrows
        def resumeApply(fa: F[A])(implicit F: Applicative[F]): Free[F, B] = {
          val zipped = F.map2(fa)(fi)((a, i) => (a, i))
          arrows.tryToFree match {
            case None =>
              val fa = F.map(zipped)(_._1)
              val fi = F.map(zipped)(_._2)
              Free.impure(fi, arrows.interleave(fa))
            case Some(free) =>
              Free.lift(zipped).map2(free) { case ((a, i), f) => f(i)(a) }
          }
        }
      }
      final case class Bind[F0[_], G[_], A, B] private (
        _f: A => Free[F0, B],
        _nt: NaturalTransformation[F0, G]
      ) extends Arrow[G, A, B] {
        type F[C] = F0[C]
        def f: A => Free[F, B] = _f
        def nt: NaturalTransformation[F, G] = _nt
      }
    }
  }

  implicit def monad[F[_]]: Monad[Free[F, *]] = new Monad[Eff[F, *]] {
    override def point[A]: (=> A) => Eff[F, A] = a => Free.point(a)
    override def ap[A, B]: Eff[F, A] => Eff[F, A => B] => Eff[F, B] = _.ap
    override def map2[A, B, C]: Eff[F, A] => Eff[F, B] => ((A, B) => C) => Eff[F, C] = fa => fb => fa.map2(fb)
    override def bind[A, B]: Eff[F, A] => (A => Eff[F, B]) => Eff[F, B] = fa => fa.flatMap
  }
}
