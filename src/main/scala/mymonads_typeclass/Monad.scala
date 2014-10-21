package mymonads_typeclass

trait Monad[A, Self[X] <: Monad[X, Self]] {

  def map[B](f: A => B): Self[B]

  def flatMap[B](f: A => Self[B]): Self[B]
}

trait MonadOps[M[X] <: Monad[X, M]] {
  def unit[A](a: A): M[A]
}
