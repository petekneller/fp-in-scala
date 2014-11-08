package mymonads_typeclass

case class StateT[S, M[_], A](run: S => M[(S, A)])

trait StateMonad[S, M[_]] extends MonadOps[M] {
  def get: M[S]

  def set(s: S): M[Unit]

  def modify(f: S => S): M[Unit]

  def map2[A, B, C](sa: M[A], sb: M[B])(f: (A, B) => C): M[C] =
    for {
      a <- sa
      b <- sb
    } yield f(a, b)

  def sequence[A](fs: List[M[A]]): M[List[A]] = {
    fs.foldLeft(unit[List[A]](Nil))((acc: M[List[A]], f: M[A]) => map2(acc, f)(_ :+ _))
  }
}

class StateTOps[S, M[_]](innerOps: MonadOps[M]) extends StateMonad[S, ({ type L[X] = StateT[S, M, X] })#L] {
  self =>

  /*  MonadOps */
  def unit[A](a: A): StateT[S, M, A] = StateT{ s => innerOps.unit((s, a)) }

  def flatMap[A, B](m: StateT[S, M, A])(f: A => StateT[S, M, B]): StateT[S, M, B] = StateT{
    (s1: S) =>
      val ima = m.run(s1)
      val imb = innerOps.flatMap(ima){
        case (s2: S, a: A) =>
          val smb = f(a)
          smb.run(s2)
      }
      imb
  }

  def map[A, B](m: StateT[S, M, A])(f: A => B): StateT[S, M, B] = StateT{
    (s1: S) =>
      val ima = m.run(s1)
      val imb = innerOps.map(ima){
        case (s2: S, a: A) =>
          val b = f(a)
          (s2, b)
      }
      imb
  }

  implicit def monadImplicit[A](m: StateT[S, M, A]): Monad[({ type L[X] = StateT[S, M, X] })#L, A] = new Monad[({ type L[X] = StateT[S, M, X] })#L, A] {
    override def map[B](f: (A) => B): StateT[S, M, B] = self.map(m)(f)
    override def flatMap[B](f: (A) => StateT[S, M, B]): StateT[S, M, B] = self.flatMap(m)(f)
  }

  /* StateMonad */
  def get: StateT[S, M, S] = StateT{ s => innerOps.unit((s, s)) }

  def set(s: S): StateT[S, M, Unit] = StateT{ _ => innerOps.unit((s, ())) }

  def modify(f: S => S): StateT[S, M, Unit] = for {
    oldS <- get
    newS = f(oldS)
    _ <- set(newS)
  } yield ()

}
