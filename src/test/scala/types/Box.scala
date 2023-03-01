package types

import test.{Box => TestBox}

object Box {
  implicit val monad: Monad[TestBox] = new Monad[TestBox] {
    override def point[A]: (=> A) => TestBox[A] = a => TestBox(a)
    override def bind[A, B]: TestBox[A] => (A => TestBox[B]) => TestBox[B] = fa => f => f(fa.value)
  }
}
