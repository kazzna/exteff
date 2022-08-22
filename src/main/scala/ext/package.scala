package object ext {
  type ~>[F[_], G[_]] = NaturalTransformation[F, G]

  type Eff[R[_], A] = Free[R, A]
}
