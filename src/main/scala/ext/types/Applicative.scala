package ext.types

trait Applicative[F[_]] extends Point[F] with Apply[F] {
  override def apply[A, B](fa: F[A])(f: F[A => B]): F[B] = map2(fa)(f)((a, f) => f(a))
  def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] = {
    val f2: A => B => C = a => b => f(a, b)
    val pf: F[A => B => C] = point(f2)
    apply(fb)(apply(fa)(pf))
  }
}
