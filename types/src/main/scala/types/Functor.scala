package types

trait Functor[F[_]] {
  def map[A, B]: (A => B) => F[A] => F[B]
}
