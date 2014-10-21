package mymonads_typeclass

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State {
    s =>
      val (a, s2) = run(s)
      val b = f(a)
      (b, s2)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State {
    s =>
      val (a, s2) = run(s)
      f(a).run(s2)
  }
}

class StateOps[S] {
  def unit[A](a: A): State[S, A] = State(s => (a, s))

  def get: State[S, S] = State(s => (s, s))

  def set[A](s: A): State[A, Unit] = State(_ => ((), s))

  def modify(f: S => S): State[S, Unit] = for {
    oldS <- get
    newS = f(oldS)
    _ <- set(newS)
  } yield ()

  def map2[A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = State {
    (s: S) =>
      val (a, s2) = sa.run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
  }

  def sequence[A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldLeft(unit[List[A]](Nil))((acc: State[S, List[A]], f: State[S, A]) => map2(acc, f)(_ :+ _))
  }
}
