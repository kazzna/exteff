package ext

import scala.annotation.tailrec

trait NaturalTransformation[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]

  final def compose[H[_]](nt: NaturalTransformation[G, H]): NaturalTransformation[F, H] = this match {
    case NaturalTransformation.Reflect() => nt.asInstanceOf[NaturalTransformation[F, H]]
    case nt1 =>
      nt match {
        case NaturalTransformation.Reflect() => nt1.asInstanceOf[NaturalTransformation[F, H]]
        case nt2 => NaturalTransformation.Composed(nt1, nt2)
      }
  }
}

object NaturalTransformation {
  def reflect[F[_]]: NaturalTransformation[F, F] = Reflect()

  private final case class Reflect[F[_]]() extends NaturalTransformation[F, F] {
    override def apply[A](fa: F[A]): F[A] = fa
  }

  private final case class Composed[F[_], G0[_], H[_]](
    _nt1: NaturalTransformation[F, G0],
    _nt2: NaturalTransformation[G0, H]
  ) extends NaturalTransformation[F, H] {
    private type G[A] = G0[A]
    private def nt1: NaturalTransformation[F, G] = _nt1
    private def nt2: NaturalTransformation[G, H] = _nt2
    override def apply[A](fa: F[A]): H[A] = {
      @tailrec
      def loop[M[_], N[_]](
          ma: M[A],
          nt1: NaturalTransformation[M, N],
          nt2: NaturalTransformation[N, H]
      ): H[A] = nt1 match {
        case composed @ Composed(_, _) => loop(ma, composed.nt1, composed.nt2.compose(nt2))
        case nt1 =>
          val na = nt1(ma)
          nt2 match {
            case composed @ Composed(_, _) => loop(na, composed.nt1, composed.nt2)
            case nt2 => nt2(na)
          }
      }

      loop(fa, nt1, nt2)
    }
  }
}
