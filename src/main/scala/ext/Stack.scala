package ext

import scala.language.reflectiveCalls

sealed trait Stack[F[_], G[_], A]

object Stack {
  type of[F[_], G[_]] = { type R[A] = Stack[F, G, A] }
  type of1[F1[_]] = of[F1, Void]
  type of2[F1[_], F2[_]] = of[F1, of1[F2]#R]
  type of3[F1[_], F2[_], F3[_]] = of[F1, of2[F2, F3]#R]
  type of4[F1[_], F2[_], F3[_], F4[_]] = of[F1, of3[F2, F3, F4]#R]
  type of5[F1[_], F2[_], F3[_], F4[_], F5[_]] = of[F1, of4[F2, F3, F4, F5]#R]
  type of6[F1[_], F2[_], F3[_], F4[_], F5[_], F6[_]] = of[F1, of5[F2, F3, F4, F5, F6]#R]
  type of7[F1[_], F2[_], F3[_], F4[_], F5[_], F6[_], F7[_]] = of[F1, of6[F2, F3, F4, F5, F6, F7]#R]
  type of8[F1[_], F2[_], F3[_], F4[_], F5[_], F6[_], F7[_], F8[_]] =
    of[F1, of7[F2, F3, F4, F5, F6, F7, F8]#R]

  def top[F[_], G[_], A](fa: F[A]): Stack[F, G, A] = Top(fa)
  def body[F[_], G[_], A](ga: G[A]): Stack[F, G, A] = Body(ga)

  def extract[F[_]]: Stack.of1[F]#R ~> F = new NaturalTransformation[Stack.of1[F]#R, F] {
    override def apply[A](fa: Stack[F, Void, A]): F[A] = fa.extract
  }
  sealed trait Void[A]

  final case class Top[F[_], G[_], A] private (fa: F[A]) extends Stack[F, G, A]
  final case class Body[F[_], G[_], A] private (ga: G[A]) extends Stack[F, G, A]

  implicit class Extract[F[_], A](val stack: Stack.of1[F]#R[A]) extends AnyVal {
    def extract: F[A] = stack match {
      case Top(fa) => fa
      case Body(_) => sys.error("Void would never have instance.")
    }
  }

  def restruct[F[_], G[_], R[_]](
      nt: NaturalTransformation[F, G]
  )(implicit G: G In R): NaturalTransformation[Stack.of[F, R]#R, R] =
    new NaturalTransformation[Stack.of[F, R]#R, R] {
      override def apply[A](fa: Stack[F, R, A]): R[A] = fa match {
        case Top(fa) => G.inject(nt(fa))
        case Body(ga) => ga
      }
    }

  implicit class Restruct[F[_], R[_], A](val stack: Stack.of[F, R]#R[A]) extends AnyVal {
    def restruct[G[_]](nt: F ~> G)(implicit G: G In R): R[A] = Stack.restruct(nt).apply(stack)
  }
}
