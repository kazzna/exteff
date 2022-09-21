package types

import test.{Box => TestBox}

object Box {
  implicit val monad: Monad[TestBox] = new Monad[TestBox] {
    override def point[A](a: A): TestBox[A] = TestBox(a)
    override def bind[A, B](f: A => TestBox[B]): TestBox[A] => TestBox[B] = fa => f(fa.value)
  }
}
