package types

trait Point[F[_]] {
  def point[A](a: A): F[A]
}
