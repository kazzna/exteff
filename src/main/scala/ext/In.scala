package ext

import scala.language.reflectiveCalls

sealed trait In[F[_], G[_]] {
  def inject[A](fa: F[A]): G[A]
}

object In {
  implicit def top[F[_], G[_]]: F In Stack.of[F, G]#R = new In[F, Stack.of[F, G]#R] {
    override final def inject[A](fa: F[A]): Stack[F, G, A] = Stack.top(fa)
  }

  implicit def body[F[_], G[_], H[_]](implicit F: F In H): F In Stack.of[G, H]#R = new In[F, Stack.of[G, H]#R] {
    override final def inject[A](fa: F[A]): Stack[G, H, A] = Stack.body(F.inject(fa))
  }
}
