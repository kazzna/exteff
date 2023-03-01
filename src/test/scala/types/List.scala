package types

import scala.{List => ScalaList}

object List {
  implicit val monad: Monad[List] = new Monad[List] {
    override def point[A]: (=> A) => List[A] = a => ScalaList(a)
    override def bind[A, B]: List[A] => (A => List[B]) => List[B] = _.flatMap
  }

  implicit def semigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    override def append: List[A] => (=> List[A]) => List[A] = a => b => a ++ b
  }
}
