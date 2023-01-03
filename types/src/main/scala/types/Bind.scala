package types

trait Bind[F[_]] {
  def bind[A, B]: (A => F[B]) => F[A] => F[B]
}
