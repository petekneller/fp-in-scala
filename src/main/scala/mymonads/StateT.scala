package mymonads

case class StateT[S, A](run: S => Identity[(A, S)]) {

  def map[B](f: A => B): StateT[S, B] = StateT {
    s =>
      val innerA: Identity[(A, S)] = run(s)
      val innerB: Identity[(B, S)] = innerA.flatMap {
        case (a, s2) => Identity.unit((f(a), s2))
      }
      innerB
  }

  def flatMap[B](f: A => StateT[S, B]): StateT[S, B] = StateT {
    s =>
      val innerA: Identity[(A, S)] = run(s)
      val innerB: Identity[(B, S)] = innerA.flatMap {
        case (a, s2) =>
          val stateB: StateT[S, B] = f(a)
          stateB.run(s2)
      }
      innerB
  }
}

class StateTOps[S] {

  def unit[A](a: A): StateT[S, A] = StateT(s => Identity.unit((a, s)))

  def get: StateT[S, S] = StateT(s => Identity.unit((s, s)))

  def set[A](s: A): StateT[A, Unit] = StateT(_ => Identity.unit(((), s)))

  def modify(f: S => S): StateT[S, Unit] = for {
    oldS <- get
    newS = f(oldS)
    _ <- set(newS)
  } yield ()

  def map2[A, B, C](sa: StateT[S, A], sb: StateT[S, B])(f: (A, B) => C): StateT[S, C] =
    for {
      a <- sa
      b <- sb
    } yield f(a, b)

  def sequence[A](fs: List[StateT[S, A]]): StateT[S, List[A]] = {
    fs.foldLeft(unit[List[A]](Nil))((acc: StateT[S, List[A]], f: StateT[S, A]) => map2(acc, f)(_ :+ _))
  }

}
