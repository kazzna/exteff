package types

import test.{Box => TestBox}

object Box {
  implicit val monad: Monad[TestBox] = new Monad[TestBox] {
    override def point[A]: A => TestBox[A] = TestBox.apply
    override def bind[A, B]: (A => TestBox[B]) => TestBox[A] => TestBox[B] = f => fa => f(fa.value)
  }
}
