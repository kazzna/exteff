package ext

import ext.types.Monad

object Eff {
  def point[R[_], A](a: A): Eff[R, A] = Free.point(a)
  def inject[R[_], F[_], A](fa: F[A])(implicit F: F In R): Eff[R, A] = Free.lift(F.inject(fa))

  implicit class Single[F[_], A](val eff: Eff[Stack.of1[F]#R, A]) extends AnyVal {
    def extract(implicit M: Monad[F]): F[A] = eff.transform(Stack.extract).extract
  }

  implicit class Stacked[F[_], R[_], A](val eff: Eff[Stack.of[F, R]#R, A]) extends AnyVal {
    def restruct[G[_]](nt: NaturalTransformation[F, G])(implicit G: G In R): Eff[R, A] =
      eff.transform(Stack.restruct(nt))
  }
}
