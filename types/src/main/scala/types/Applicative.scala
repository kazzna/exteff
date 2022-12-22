package types

trait Applicative[F[_]] extends Functor[F] with Point[F] with Apply[F] {
  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(point(f))
  override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = map2(fa)(f)((a, f) => f(a))
  def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] = ap(fb)(ap(fa)(point(f.curried)))
}
