package types

object Either {
  implicit def monadWithLeftSemigroup[E](implicit E: Semigroup[E]): Monad[Either[E, *]] = new Monad[Either[E, *]] {
    override def point[A](a: A): Either[E, A] = Right(a)
    override def ap[A, B](f: Either[E, A => B]): Either[E, A] => Either[E, B] =
      _.left.map(e1 => f.left.map(e2 => E.append(e1, e2)).map(_ => e1).merge).flatMap(a => f.map(_(a)))
    override def map2[A, B, C](f: (A, B) => C): Either[E, A] => Either[E, B] => Either[E, C] = fa =>
      fb => ap(fb.map(b => (f: B => C) => f(b)))(fa.map(f.curried))
    override def bind[A, B](f: A => Either[E, B]): Either[E, A] => Either[E, B] = _.flatMap(f)
  }
}
