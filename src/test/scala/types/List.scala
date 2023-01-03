package types

import scala.{List => ScalaList}

object List {
  implicit val monad: Monad[List] = new Monad[List] {
    override def point[A]: A => List[A] = a => ScalaList(a)
    override def bind[A, B]: (A => List[B]) => List[A] => List[B] = f => _.flatMap(f)
  }

  implicit def semigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    override def append: (List[A], List[A]) => List[A] = { case (a, b) =>
      a ++ b
    }
  }
}
