package ext

import scala.language.reflectiveCalls

sealed trait Stack[F[_], G[_], A]

object Stack {
  final case class Top[F[_], G[_], A](fa: F[A]) extends Stack[F, G, A]
  final case class Body[F[_], G[_], A](ga: G[A]) extends Stack[F, G, A]

  sealed trait Void[A]

  type of[F[_], G[_]] = {type R[A] = Stack[F, G, A]}

  type of1[F1[_]] = of[F1, Void]
  type of2[F1[_], F2[_]] = of[F1, of1[F2]#R]
  type of3[F1[_], F2[_], F3[_]] = of[F1, of2[F2, F3]#R]
  type of4[F1[_], F2[_], F3[_], F4[_]] = of[F1, of3[F2, F3, F4]#R]
  type of5[F1[_], F2[_], F3[_], F4[_], F5[_]] = of[F1, of4[F2, F3, F4, F5]#R]
  type of6[F1[_], F2[_], F3[_], F4[_], F5[_], F6[_]] = of[F1, of5[F2, F3, F4, F5, F6]#R]
  type of7[F1[_], F2[_], F3[_], F4[_], F5[_], F6[_], F7[_]] = of[F1, of6[F2, F3, F4, F5, F6, F7]#R]
  type of8[F1[_], F2[_], F3[_], F4[_], F5[_], F6[_], F7[_], F8[_]] =
    of[F1, of7[F2, F3, F4, F5, F6, F7, F8]#R]
}
