package mymonads_typeclass

trait Monad[M[_], A] {
  def map[B](f: A => B): M[B]

  def flatMap[B](f: A => M[B]): M[B]
}


trait MonadOps[M[_]] {
  def unit[A](a: A): M[A]

  def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]

  def map[A, B](m: M[A])(f: A => B): M[B]

  implicit def monadImplicit[A](m: M[A]): Monad[M, A]
}
