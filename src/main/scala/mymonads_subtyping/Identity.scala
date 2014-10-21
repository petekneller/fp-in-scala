package mymonads_subtyping

case class Identity[A](run: A) extends Monad[A, Identity] {

  def map[B](f: A => B): Identity[B] = Identity(f(run))

  def flatMap[B](f: A => Identity[B]): Identity[B] = Identity(f(run).run)

}

class IdentityOps extends MonadOps[Identity] {

  def unit[A](a: A): Identity[A] = Identity(a)

}
