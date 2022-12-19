package ext

sealed abstract class IndexedState[S, T, A] {
  def run(s: S): (A, T)
  def map[B](f: A => B): IndexedState[S, T, B] = flatMap(a => IndexedState.pure(f(a)))
  def flatMap[U, B](f: A => IndexedState[T, U, B]): IndexedState[S, U, B] = this match {
    case primitive: IndexedState.Primitive[S, T, A] => IndexedState.FlatMap(primitive, f)
    case IndexedState.FlatMap(si, arrows) => IndexedState.FlatMap(si, arrows.andThen(_.flatMap(f)))
  }
}

object IndexedState {
  @inline
  def pure[S, A](a: A): IndexedState[S, S, A] = point(a)
  def point[S, A](a: A): IndexedState[S, S, A] = Point(a)
  def get[S]: IndexedState[S, S, S] = Get()
  def put[S, T](t: T): IndexedState[S, T, Unit] = Put(t)
  def modify[S, T](eval: S => T): IndexedState[S, T, Unit] = for {
    s <- get[S]
    _ <- put(eval(s))
  } yield ()
  def apply[S, T, A](run: S => (A, T)): IndexedState[S, T, A] = for {
    s <- get[S]
    (a, t) = run(s)
    _ <- put(t)
  } yield a

  sealed abstract class Primitive[S, T, A] extends IndexedState[S, T, A] {
    override final def run(s: S): (A, T) = runPrimitive(s)
    def runPrimitive(s: S): (A, T)
  }
  final case class Point[S, A] private (a: A) extends Primitive[S, S, A] {
    override def runPrimitive(s: S): (A, S) = (a, s)
  }
  final case class Get[S] private () extends Primitive[S, S, S] {
    override def runPrimitive(s: S): (S, S) = (s, s)
  }
  final case class Put[S, T] private (t: T) extends Primitive[S, T, Unit] {
    override def runPrimitive(s: S): (Unit, T) = ((), t)
  }
  final case class FlatMap[S, T, U, I, A](
    si: Primitive[S, T, I],
    arrows: I => IndexedState[T, U, A]
  ) extends IndexedState[S, U, A] {
    override def run(s: S): (A, U) = {
      val (i, t) = si.run(s)
      val state = arrows(i)
      state.run(t)
    }
  }
}
