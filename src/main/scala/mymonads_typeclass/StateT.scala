package mymonads_typeclass

case class StateT[S, M[_]: MonadOps, A](run: S => M[(S, A)]) {
  val innerOps = implicitly[MonadOps[M]]

  def map[B](f: A => B): StateT[S, M, B] = StateT{
    (s1: S) =>
      val ima = run(s1)
      val imb = innerOps.map(ima){
        case (s2: S, a: A) =>
          val b = f(a)
          (s2, b)
      }
      imb
  }

  def flatMap[B](f: A => StateT[S, M, B]): StateT[S, M, B] = StateT{
    (s1: S) =>
      val ima = run(s1)
      val imb = innerOps.flatMap(ima){
        case (s2: S, a: A) =>
          val smb = f(a)
          smb.run(s2)
      }
      imb
  }
}

class StateTOps[S, M[_]](innerOps: MonadOps[M]) {
  private implicit val iOps = innerOps

  def unit[A](a: A): StateT[S, M, A] = StateT{ s => innerOps.unit((s, a)) }

  def get: StateT[S, M, S] = StateT{ s => innerOps.unit((s, s)) }

  def set(s: S): StateT[S, M, Unit] = StateT{ _ => innerOps.unit((s, ())) }

  def modify(f: S => S): StateT[S, M, Unit] = for {
    oldS <- get
    newS = f(oldS)
    _ <- set(newS)
  } yield ()

  def map2[A, B, C](sa: StateT[S, M, A], sb: StateT[S, M, B])(f: (A, B) => C): StateT[S, M, C] =
    for {
      a <- sa
      b <- sb
    } yield f(a, b)

  def sequence[A](fs: List[StateT[S, M, A]]): StateT[S, M, List[A]] = {
    fs.foldLeft(unit[List[A]](Nil))((acc: StateT[S, M, List[A]], f: StateT[S, M, A]) => map2(acc, f)(_ :+ _))
  }

}
