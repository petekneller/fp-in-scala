package mymonads_typeclass

case class Identity[A](run: A) extends Monad[Identity, A] {

  def map[B](f: A => B): Identity[B] = Identity(f(run))

  def flatMap[B](f: A => Identity[B]): Identity[B] = Identity(f(run).run)

}

class IdentityOps extends MonadOps[Identity] {

  def unit[A](a: A): Identity[A] = Identity(a)

  def flatMap[A, B](m: Identity[A])(f: A => Identity[B]): Identity[B] = m.flatMap(f)

  def map[A, B](m: Identity[A])(f: A => B): Identity[B] = m.map(f)

  implicit def monadImplicit[A](m: Identity[A]): Monad[Identity, A] = m
}
