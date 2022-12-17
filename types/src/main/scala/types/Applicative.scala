package types

trait Applicative[F[_]] extends Functor[F] with Point[F] with Apply[F] {
  override def map[A, B](f: A => B): F[A] => F[B] = fa => ap(point(f))(fa)
  override def ap[A, B](f: F[A => B]): F[A] => F[B] = fa => map2(fa)(f)((a, f) => f(a))
  def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] = ap(ap(point(f.curried))(fa))(fb)
}
