package mymonads

case class Identity[A](run: A) {

  def map[B](f: A => B): Identity[B] = Identity(f(run))

  def flatMap[B](f: A => Identity[B]): Identity[B] = Identity(f(run).run)

}

object Identity {

  def unit[A](a: A): Identity[A] = Identity(a)

}
