package ext.types

import scala.concurrent.{ExecutionContext, Future}

object FutureMonad {
  implicit def monadInstance(implicit ec: ExecutionContext): Monad[Future] = new Monad[Future] {
    override def point[A](a: A): Future[A] = Future.successful(a)
    override def map2[A, B, C](fa: Future[A])(fb: Future[B])(f: (A, B) => C): Future[C] =
      fa.zipWith(fb)(f)
    override def apply[A, B](fa: Future[A])(f: Future[A => B]): Future[B] =
      fa.zipWith(f)((a, f) => f(a))
    override def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)
  }
}
