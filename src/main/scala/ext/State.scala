package ext

import scala.annotation.tailrec

sealed abstract class State[S, A] {
  @tailrec
  final def run(s: S): (A, S) = this match {
    case State.Pure(a) => (a, s)
    case State.Impure(op, arrows) =>
      op match {
        case State.Get() => arrows.asInstanceOf[S => State[S, A]](s).run(s)
        case State.Put(s) => arrows.asInstanceOf[Unit => State[S, A]](()).run(s)
      }
  }
  final def map[B](f: A => B): State[S, B] = this match {
    case State.Pure(a) => State.pure(f(a))
    case State.Impure(op, arrows) => State.impure(op, arrows.andThen(_.map(f)))
  }
  final def ap[B](f: State[S, A => B]): State[S, B] =
    this.flatMap(a => f.map(f => f(a)))
  final def map2[B, C](state: State[S, B])(f: (A, B) => C): State[S, C] =
    map(f.curried).flatMap(f => state.map(b => f(b)))
  final def flatMap[B](f: A => State[S, B]): State[S, B] = this match {
    case State.Pure(a) => f(a)
    case State.Impure(op, arrows) => State.impure(op, arrows.andThen(_.flatMap(f)))
  }
  final def eval(s: S): A = run(s)._1
  final def exec(s: S): S = run(s)._2
}

object State {
  def pure[S, A](a: A): State[S, A] = Pure(a)
  @inline
  private def impure[S, I, A](op: Operate[S, I], arrows: I => State[S, A]): State[S, A] = Impure(op, arrows)
  def apply[S, A](run: S => (A, S)): State[S, A] = for {
    s <- get[S]
    (a, s2) = run(s)
    _ <- put(s2)
  } yield a
  @inline
  def point[S, A](a: A): State[S, A] = pure(a)
  def get[S]: State[S, S] = impure(Get(), (s: S) => pure(s))
  def put[S](s: S): State[S, Unit] = impure(Put(s), (_: Unit) => pure(()))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get[S]
    _ <- put(f(s))
  } yield ()

  sealed abstract class Operate[S, +A]
  final case class Get[S] private () extends Operate[S, S]
  final case class Put[S] private (s: S) extends Operate[S, Unit]

  final case class Pure[S, A](a: A) extends State[S, A]
  final case class Impure[S, I, A](op: Operate[S, I], arrows: I => State[S, A]) extends State[S, A]
}
