package types

trait Apply[F[_]] {
  def ap[A, B](f: F[A => B]): F[A] => F[B]
}
