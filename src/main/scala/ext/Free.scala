package ext

import ext.types.{Applicative, Monad}

sealed trait Free[F[_], A] {
  def map[B](f: A => B): Free[F, B] = this match {
    case Free.Pure(a) => Free.point(f(a))
    case Free.Impure(ri, arrows) => Free.Impure(ri, arrows.thenApply(Free.point(f)))
  }

  def map2[B, C](fb: Free[F, B])(f: (A, B) => C): Free[F, C] =
    fb.ap(ap(Free.pure[F, A => B => C](f.curried)))

  def ap[B](f: Free[F, A => B]): Free[F, B] = this match {
    case Free.Pure(a) => f.map(f => f(a))
    case Free.Impure(ri, arrows) => Free.impure(ri, arrows.thenApply(f))
  }

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
    case Free.Pure(a) => f(a)
    case Free.Impure(ri, arrows) => Free.impure(ri, arrows.thenBind(f))
  }

  def bindArrows[B](arrows: Arrows[F, A, B]): Free[F, B] = this match {
    case Free.Pure(a) => arrows.resume(a)
    case Free.Impure(ri, arrows0) => Free.impure(ri, arrows0.andThen(arrows))
  }

  def transform[G[_]](nt: NaturalTransformation[F, G]): Free[G, A] = this match {
    case Free.Pure(a) => Free.pure(a)
    case Free.Impure(ri, arrows) => Free.impure(nt(ri), arrows.transform(nt))
  }

  def extract(implicit F: Monad[F]): F[A] = this match {
    case Free.Pure(a) => F.point(a)
    case Free.Impure(ri, arrows) => arrows.extract(ri)
  }

  def extractApplicative(implicit F: Applicative[F]): Either[Free[F, A], F[A]] = this match {
    case Free.Pure(a) => Right(F.point(a))
    case Free.Impure(ri, arrows) => arrows.extractApplicative(ri)
  }

  def foldMap[G[_]](nt: NaturalTransformation[F, G])(implicit M: Monad[G]): G[A] = transform(nt).extract
}

object Free {
  def pure[F[_], A](a: A): Free[F, A] = Pure(a)
  def impure[F[_], I, A](ri: F[I], arrow: Arrows[F, I, A]): Free[F, A] = Impure(ri, arrow)

  @inline
  def point[F[_], A](a: A): Free[F, A] = pure(a)
  def lift[F[_], A](fa: F[A]): Free[F, A] = impure(fa, Arrows.point)

  final case class Pure[F[_], A] private (a: A) extends Free[F, A]
  final case class Impure[F[_], I, A] private (ri: F[I], arrows: Arrows[F, I, A]) extends Free[F, A]

  implicit class FreeAp[F[_], A, B](val free: Free[F, A => B]) extends AnyVal {
    def apply(fa: Free[F, A]): Free[F, B] = fa.ap(free)
  }
}
