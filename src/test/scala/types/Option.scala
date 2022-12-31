package types

object Option {
  implicit val monad: Monad[Option] = new Monad[Option] {
    override def point[A](a: A): Option[A] = Some(a)
    override def bind[A, B](f: A => Option[B]): Option[A] => Option[B] = _.flatMap(f)
  }
}
