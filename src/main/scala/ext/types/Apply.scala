package ext.types

trait Apply[F[_]] {
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]
}
