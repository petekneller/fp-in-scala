//package mymonads
//
//// my thoughts with this trait were that StateT should extend it, and StateTOps return it, so
//// uses of StateT and StateTOps didnt need to know about eh concrete type
//// but im not sure about how to deal with losing the type info for the inner monad
//trait StateMonad[S, A, Self[X] <: StateMonad[S, X, Self]] extends Monad[A, ({ type L[X] = StateMonad[S, X, Self] })#L]
//
//case class StateT[S, M[X] <: Monad[X, M] : MonadOps, A](run: S => M[(A, S)]) extends StateMonad[S, A, ({ type L[Y] = StateT[S, M, Y] })#L] {
//  val innerOps = implicitly[MonadOps[M]]
//
//  def map[B](f: A => B): StateT[S, M, B] = StateT {
//    s =>
//      val innerA: M[(A, S)] = run(s)
//      val innerB: M[(B, S)] = innerA.flatMap {
//        case (a, s2) =>
//        val b = f(a)
//        innerOps.unit((b, s2))
//      }
//      innerB
//  }
//
//  def flatMap[B](f: A => StateT[S, M, B]): StateT[S, M, B] = StateT {
//    s =>
//      val innerA: M[(A, S)] = run(s)
//      val innerB: M[(B, S)] = innerA.flatMap {
//        case (a, s2) =>
//          val stateB: StateT[S, M, B] = f(a)
//          stateB.run(s2)
//      }
//      innerB
//  }
//}
//
//trait StateTOps[S, M[X] <: StateMonad[S, X, M]] {
//  def unit[A](a: A): M[A]
//
//  def get: M[S]
//
//  def set[S](s: S): M[Unit]
//
//  def modify(f: S => S): M[Unit]
//
//  def map2[A, B, C](sa: M[A], sb: M[B])(f: (A, B) => C): M[C]
//
//  def sequence[A](fs: List[M[A]]): M[List[A]]
//}
//
//class StateTOpsImpl[S, M[X] <: Monad[X, M]](innerOps: MonadOps[M]) extends StateTOps[S, ({ type L[X] = StateT[S, M, X] })#L] {
//  implicit val io = innerOps
//
//  def unit[A](a: A): StateT[S, M, A] = StateT(s => innerOps.unit((a, s)))
//
//  def get: StateT[S, M, S] = StateT(s => innerOps.unit((s, s)))
//
//  def set[S](s: S): StateT[S, M, Unit] = StateT(_ => innerOps.unit(((), s)))
//
//  def modify(f: S => S): StateT[S, M, Unit] = for {
//    oldS <- get
//    newS = f(oldS)
//    _ <- set(newS)
//  } yield ()
//
//  def map2[A, B, C](sa: StateT[S, M, A], sb: StateT[S, M, B])(f: (A, B) => C): StateT[S, M, C] =
//    for {
//      a <- sa
//      b <- sb
//    } yield f(a, b)
//
//  def sequence[A](fs: List[StateT[S, M, A]]): StateT[S, M, List[A]] = {
//    fs.foldLeft(unit[List[A]](Nil))((acc: StateT[S, M, List[A]], f: StateT[S, M, A]) => map2(acc, f)(_ :+ _))
//  }
//
//}
