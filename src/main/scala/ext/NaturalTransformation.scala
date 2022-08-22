package ext

import scala.annotation.tailrec

trait NaturalTransformation[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]

  final def compose[H[_]](nt: G ~> H): NaturalTransformation[F, H] = (this, nt) match {
    case (nt1, NaturalTransformation.Reflect()) => nt1.asInstanceOf[F ~> H]
    case (NaturalTransformation.Reflect(), nt2) => nt2.asInstanceOf[F ~> H]
    case (nt1, nt2) => NaturalTransformation.Composed(nt1, nt2)
  }
}

object NaturalTransformation {
  def reflect[F[_]]: NaturalTransformation[F, F] = Reflect()

  private final case class Reflect[F[_]]() extends NaturalTransformation[F, F] {
    override def apply[A](fa: F[A]): F[A] = fa
  }

  private final case class Composed[F[_], G[_], H[_]](
    left: F ~> G,
    right: G ~> H
  ) extends NaturalTransformation[F, H] {
    override def apply[A](fa: F[A]): H[A] = sorted.apply(fa)

    private def sorted: NaturalTransformation[F, H] = {
      @tailrec
      def loop[I[_]](a: F ~> I, b: I ~> H): F ~> H = (a, b) match {
        case (_, Composed(ba, bb)) => loop(a.compose(ba), bb)
        case (Composed(aa, ab), _) =>
          ab match {
            case Composed(aba, abb) => loop(aa.compose(aba), abb.compose(b))
            case _ => loop(aa, Sorted(ab, b))
          }
        case _ => Sorted(a, b)
      }

      loop(left, right)
    }
  }

  private final case class Sorted[F[_], G[_], H[_]](
    first: F ~> G,
    next: G ~> H
  ) extends NaturalTransformation[F, H] {
    override def apply[A](fa: F[A]): H[A] = {
      @tailrec
      def loop[I[_], J[_]](nt1: I ~> J, nt2: J ~> H, acc: I[A]): H[A] = nt2 match {
        case Sorted(first, next) => loop(first, next, nt1(acc))
        case nt => nt(nt1(acc))
      }

      loop(first, next, fa)
    }
  }
}
