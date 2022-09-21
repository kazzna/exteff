package types

import scala.{List => ScalaList}

object List {
  implicit val monad: Monad[List] = new Monad[List] {
    override def point[A](a: A): List[A] = ScalaList(a)
    override def bind[A, B](f: A => List[B]): List[A] => List[B] = _.flatMap(f)
  }

  implicit def semigroup[A]: Semigroup[List[A]] = (a1: List[A], a2: List[A]) => a1 ++ a2
}
