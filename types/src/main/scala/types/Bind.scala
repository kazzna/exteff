package types

trait Bind[F[_]] {
  def bind[A, B](f: A => F[B]): F[A] => F[B]
}
