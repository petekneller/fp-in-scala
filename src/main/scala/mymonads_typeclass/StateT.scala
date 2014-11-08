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

trait StateMonad[S, M[_, _]] extends MonadOps[({ type L[X] = M[S, X] })#L] {
  def unit[A](a: A): M[S, A]

  def get: M[S, S]

  def set(s: S): M[S, Unit]

  def modify(f: S => S): M[S, Unit]

  def map2[A, B, C](sa: M[S, A], sb: M[S, B])(f: (A, B) => C): M[S, C]

  def sequence[A](fs: List[M[S, A]]): M[S, List[A]]
}

class StateTOps[S, M[_]](innerOps: MonadOps[M]) extends StateMonad[S, ({ type L[X, Y] = StateT[X, M, Y] })#L] {
  private implicit val iOps = innerOps

  /*  MonadOps */
  def unit[A](a: A): StateT[S, M, A] = StateT{ s => innerOps.unit((s, a)) }

  def flatMap[A, B](m: StateT[S, M, A])(f: A => StateT[S, M, B]): StateT[S, M, B] = m.flatMap(f)

  def map[A, B](m: StateT[S, M, A])(f: A => B): StateT[S, M, B] = m.map(f)

  implicit def monadImplicit[A](m: StateT[S, M, A]): Monad[({ type L[X] = StateT[S, M, X] })#L, A] = new Monad[({ type L[X] = StateT[S, M, X] })#L, A] {
    override def map[B](f: (A) => B): StateT[S, M, B] = m.map(f)
    override def flatMap[B](f: (A) => StateT[S, M, B]): StateT[S, M, B] = m.flatMap(f)
  }

  /* StateMonad */
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
