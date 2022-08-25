package ext.types

trait Bind[F[_]] {
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
}
