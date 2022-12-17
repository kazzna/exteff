package types

object OptionMonad {
  implicit val monadInstance: Monad[Option] = new Monad[Option] {
    override def point[A](a: A): Option[A] = Some(a)
    override def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }
}
