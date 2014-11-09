package mymonads_typeclass

case class WriterT[W, M[_], A](run: (List[W] => M[(List[W], A)]))

trait WriterMonad[W, M[_]] extends MonadOps[M] {

  def write(w: W): M[Unit]
}

class WriterTOps[W, M[_]](innerOps: MonadOps[M]) extends WriterMonad[W, ({ type L[X] = WriterT[W, M, X] })#L] with MonadTrans[M, ({ type L[X] = WriterT[W, M, X] })#L] {
  self =>

  /* MonadOps */
  def unit[A](a: A): WriterT[W, M, A] = WriterT{ w => innerOps.unit((w, a)) }

  def flatMap[A, B](m: WriterT[W, M, A])(f: (A) => WriterT[W, M, B]): WriterT[W, M, B] = WriterT{
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

  def map[A, B](m: WriterT[W, M, A])(f: (A) => B): WriterT[W, M, B] = flatMap(m)(a => unit(f(a)))

  implicit def monadImplicit[A](m: WriterT[W, M, A]): Monad[({type L[X] = WriterT[W, M, X]})#L, A] = new Monad[({type L[X] = WriterT[W, M, X]})#L, A] {
    override def map[B](f: (A) => B): WriterT[W, M, B] = self.map(m)(f)
    override def flatMap[B](f: (A) => WriterT[W, M, B]): WriterT[W, M, B] = self.flatMap(m)(f)
  }

  /* WriterMonad */
  def write(w: W): WriterT[W, M, Unit] = WriterT{
    ws =>
      innerOps.unit((w :: ws, ()))
  }

  /* MonadTrans */
  def lift[A](m: M[A]): WriterT[W, M, A] = WriterT{ w => innerOps.flatMap(m){ a => innerOps.unit((w, a)) } }

}

object WriterTOps {

  def toWriterMonad[W, IM[_], OM[_]](outerOps: MonadTrans[IM, OM], innerWriter: WriterMonad[W,  IM]): WriterMonad[W, OM] = new WriterMonad[W, OM] {

    /* MonadOps */
    override def flatMap[A, B](m: OM[A])(f: (A) => OM[B]): OM[B] = outerOps.flatMap(m)(f)

    override def unit[A](a: A): OM[A] = outerOps.unit(a)

    override def map[A, B](m: OM[A])(f: (A) => B): OM[B] = outerOps.map(m)(f)

    override implicit def monadImplicit[A](m: OM[A]): Monad[OM, A] = outerOps.monadImplicit(m)

    /* WriterMonad */
    override def write(w: W): OM[Unit] = outerOps.lift(innerWriter.write(w))
  }

}
