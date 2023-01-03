package types

object Either {
  implicit def monadWithLeftSemigroup[E](implicit E: Semigroup[E]): Monad[Either[E, *]] = new Monad[Either[E, *]] {
    override def point[A]: A => Either[E, A] = Right.apply
    override def ap[A, B]: Either[E, A => B] => Either[E, A] => Either[E, B] = f =>
      _.left.map(e1 => f.left.map(e2 => E.append(e1, e2)).map(_ => e1).merge).flatMap(a => f.map(_(a)))
    override def map2[A, B, C]: ((A, B) => C) => Either[E, A] => Either[E, B] => Either[E, C] = f =>
      fa => fb => ap(fb.map(b => (f: B => C) => f(b)))(fa.map(f.curried))
    override def bind[A, B]: (A => Either[E, B]) => Either[E, A] => Either[E, B] = f => _.flatMap(f)
  }
}
