package types

trait Applicative[F[_]] extends Functor[F] with Point[F] with Apply[F] {
  override def map[A, B](f: A => B): F[A] => F[B] = fa => ap(point(f))(fa)
  override def ap[A, B](f: F[A => B]): F[A] => F[B] = fa => map2((a: A, f: A => B) => f(a))(fa)(f)
  def map2[A, B, C](f: (A, B) => C): F[A] => F[B] => F[C] = fa => fb => ap(ap(point(f.curried))(fa))(fb)
}
