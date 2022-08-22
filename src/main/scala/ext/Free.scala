package ext

import ext.types.Monad

sealed trait Free[F[_], A] {
  def map[B](f: A => B): Free[F, B] = flatMap(a => Free.point(f(a)))

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
    case Free.Pure(a) => f(a)
    case Free.Impure(ri, arrow) => Free.impure(ri, arrow.thenBind(f))
  }

  def bindArrows[B](arrows: Arrows[F, A, B]): Free[F, B] = this match {
    case Free.Pure(a) => arrows.resume(a)
    case Free.Impure(ri, arrow) => Free.impure(ri, arrow.andThen(arrows))
  }

  def transform[G[_]](nt: NaturalTransformation[F, G]): Free[G, A] = this match {
    case Free.Pure(a) => Free.pure(a)
    case Free.Impure(ri, arrow) => Free.impure(nt(ri), arrow.transform(nt))
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
  final case class Impure[F[_], I, A] private (ri: F[I], arrow: Arrows[F, I, A]) extends Free[F, A]

  implicit class Extract[F[_], A](val eff: Free[F, A]) extends AnyVal {
    def extract(implicit F: Monad[F]): F[A] = eff match {
      case Free.Pure(a) => F.point(a)
      case Free.Impure(ri, arrow) => F.bind(ri)(a => arrow.resume(a).extract)
    }
  }
}
