package types

trait Monad[F[_]] extends Applicative[F] with Bind[F] { self =>
  override def ap[A, B](f: F[A => B]): F[A] => F[B] = fa => bind(fa)(a => bind(f)(f => point(f(a))))

  override def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] =
    bind(fa)(a => bind(fb)(b => point(f.curried(a)(b))))
}
