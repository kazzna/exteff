package types

object Option {
  implicit val monad: Monad[Option] = new Monad[Option] {
    override def point[A]: (=> A) => Option[A] = a => Some(a)
    override def bind[A, B]: Option[A] => (A => Option[B]) => Option[B] = fa => fa.flatMap
  }
}
