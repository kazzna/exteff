package types

trait Monad[F[_]] extends Applicative[F] with Bind[F] { self =>
  override def ap[A, B]: F[A => B] => F[A] => F[B] = f => bind((a: A) => bind((f: A => B) => point(f(a)))(f))

  override def map2[A, B, C]: ((A, B) => C) => F[A] => F[B] => F[C] = f =>
    fa => fb => bind((a: A) => bind((b: B) => point(f.curried(a)(b)))(fb))(fa)
}
