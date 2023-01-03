package types

trait Applicative[F[_]] extends Functor[F] with Point[F] with Apply[F] {
  override def map[A, B]: (A => B) => F[A] => F[B] = f => ap(point(f))
  override def ap[A, B]: F[A => B] => F[A] => F[B] = f => fa => map2((a: A, f: A => B) => f(a))(fa)(f)
  def map2[A, B, C]: ((A, B) => C) => F[A] => F[B] => F[C] = f => fa => ap(ap(point(f.curried))(fa))
}
