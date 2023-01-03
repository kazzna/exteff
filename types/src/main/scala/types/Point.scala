package types

trait Point[F[_]] {
  def point[A]: A => F[A]
}
