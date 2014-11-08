package mymonads_typeclass

case class WriterT[W, M[_], A](run: (List[W] => M[(List[W], A)]))

class WriterTOps[W, M[_]](innerOps: MonadOps[M]) extends MonadOps[({ type L[X] = WriterT[W, M, X] })#L] {
  self =>

  override def unit[A](a: A): WriterT[W, M, A] = WriterT{ w => innerOps.unit((w, a)) }

  override def flatMap[A, B](m: WriterT[W, M, A])(f: (A) => WriterT[W, M, B]): WriterT[W, M, B] = WriterT{
    w =>
      val ima = m.run(w)
      val imb = innerOps.flatMap(ima){
        case (w2, a) =>
          val mb = f(a)
          val imb = mb.run(w2)
          imb
      }
      imb
  }

  override def map[A, B](m: WriterT[W, M, A])(f: (A) => B): WriterT[W, M, B] = flatMap(m)(a => unit(f(a)))

  override implicit def monadImplicit[A](m: WriterT[W, M, A]): Monad[({type L[X] = WriterT[W, M, X]})#L, A] = new Monad[({type L[X] = WriterT[W, M, X]})#L, A] {
    override def map[B](f: (A) => B): WriterT[W, M, B] = self.map(m)(f)
    override def flatMap[B](f: (A) => WriterT[W, M, B]): WriterT[W, M, B] = self.flatMap(m)(f)
  }

}

object WriterTOps {

  def toStateMonad[S, W, M[_]](writerTOps: WriterTOps[W, M], innerState: StateMonad[S, M]): StateMonad[S, ({ type L[X] = WriterT[W, M, X] })#L] = new StateMonad[S, ({ type L[X] = WriterT[W, M, X] })#L] {
    /* MonadOps */
    override def unit[A](a: A): WriterT[W, M, A] = writerTOps.unit(a)

    override def flatMap[A, B](m: WriterT[W, M, A])(f: (A) => WriterT[W, M, B]): WriterT[W, M, B] = writerTOps.flatMap(m)(f)

    override def map[A, B](m: WriterT[W, M, A])(f: (A) => B): WriterT[W, M, B] = writerTOps.map(m)(f)

    override implicit def monadImplicit[A](m: WriterT[W, M, A]): Monad[({type L[X] = WriterT[W, M, X]})#L, A] = writerTOps.monadImplicit(m)

    /* StateMonad */
    override def get: WriterT[W, M, S] = WriterT{ w => innerState.flatMap(innerState.get){ s => innerState.unit((w, s)) }  }

    override def modify(f: (S) => S): WriterT[W, M, Unit] = WriterT{ w => innerState.flatMap(innerState.modify(f)){ s => innerState.unit((w, s)) } }

    override def set(s: S): WriterT[W, M, Unit] = WriterT{ w => innerState.flatMap(innerState.set(s)){ s => innerState.unit((w, s)) } }
  }
}
