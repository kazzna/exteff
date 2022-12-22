package types

trait Monad[F[_]] extends Applicative[F] with Bind[F] { self =>
  override def ap[A, B](f: F[A => B]): F[A] => F[B] = fa => bind((a: A) => bind((f: A => B) => point(f(a)))(f))(fa)

  override def map2[A, B, C](f: (A, B) => C): F[A] => F[B] => F[C] = fa =>
    fb => bind((a: A) => bind((b: B) => point(f.curried(a)(b)))(fb))(fa)
}
