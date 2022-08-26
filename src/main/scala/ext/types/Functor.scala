package ext.types

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
