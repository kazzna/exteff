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

  private final case class Composed[F[_], G[_], H[_]](
    left: NaturalTransformation[F, G],
    right: NaturalTransformation[G, H]
  ) extends NaturalTransformation[F, H] {
    override def apply[A](fa: F[A]): H[A] = sorted.apply(fa)

    private def sorted: NaturalTransformation[F, H] = {
      @tailrec
      def loop[I[_]](
          a: NaturalTransformation[F, I],
          b: NaturalTransformation[I, H]
      ): NaturalTransformation[F, H] = b match {
        case Composed(ba, bb) => loop(a.compose(ba), bb)
        case b =>
          a match {
            case Composed(aa, ab) =>
              ab match {
                case Composed(aba, abb) => loop(aa.compose(aba).compose(abb), b)
                case Sorted(aba, abb) => loop(aa.compose(aba).compose(abb), b)
                case ab => loop(aa, Sorted(ab, b))
              }
            case Sorted(aa, ab) => loop(aa.compose(ab), b)
            case a => Sorted(a, b)
          }
      }

      loop(left, right)
    }
  }

  private final case class Sorted[F[_], G[_], H[_]](
    first: NaturalTransformation[F, G],
    next: NaturalTransformation[G, H]
  ) extends NaturalTransformation[F, H] {
    override def apply[A](fa: F[A]): H[A] = {
      @tailrec
      def loop[I[_], J[_]](nt1: I ~> J, nt2: J ~> H, acc: I[A]): H[A] = nt2 match {
        case Sorted(first, next) => loop(first, next, nt1(acc))
        case nt2 => nt2(nt1(acc))
      }

      loop(first, next, fa)
    }
  }
}
