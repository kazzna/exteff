package types

trait Apply[F[_]] {
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
}
