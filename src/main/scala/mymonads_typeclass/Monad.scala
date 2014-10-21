package mymonads_typeclass



trait MonadOps[M[_]] {
  def unit[A](a: A): M[A]

  def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]

  def map[A, B](m: M[A])(f: A => B): M[B]

}
