package types

object Either {
  implicit def monadWithLeftSemigroup[E](implicit E: Semigroup[E]): Monad[Either[E, *]] = new Monad[Either[E, *]] {
    override def point[A]: (=> A) => Either[E, A] = a => Right(a)
    override def ap[A, B]: Either[E, A] => Either[E, A => B] => Either[E, B] = fa =>
      f => fa.left.map(e1 => f.left.map(e2 => E.append(e1)(e2)).map(_ => e1).merge).flatMap(a => f.map(_(a)))
    override def map2[A, B, C]: Either[E, A] => Either[E, B] => ((A, B) => C) => Either[E, C] = fa =>
      fb => f => ap(fa.map(f.curried))(fb.map(b => (f: B => C) => f(b)))
    override def bind[A, B]: Either[E, A] => (A => Either[E, B]) => Either[E, B] = fa => fa.flatMap
  }
}
