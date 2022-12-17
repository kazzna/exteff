package types

import scala.concurrent.{ExecutionContext, Future}

object FutureMonad {
  implicit def monadInstance(implicit ec: ExecutionContext): Monad[Future] = new Monad[Future] {
    override def point[A](a: A): Future[A] = Future.successful(a)
    override def map2[A, B, C](f: (A, B) => C): Future[A] => Future[B] => Future[C] = fa => fb => fa.zipWith(fb)(f)
    override def ap[A, B](f: Future[A => B]): Future[A] => Future[B] = fa => fa.zipWith(f)((a, f) => f(a))
    override def bind[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)
  }

  implicit def applicativeInstance(implicit ec: ExecutionContext): Applicative[Future] = monadInstance
}
