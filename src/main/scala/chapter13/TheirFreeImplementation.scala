package chapter13

// taken from the text book

trait Par[A] // from earlier in the book, here only for compilation
object Par {
  def unit[A](a: A): Par[A] = ???
  def flatMap[A, B](a: Par[A])(k: A => Par[B]): Par[B] = ???
  def lazyUnit[A](a: => A): Par[A] = ???
}

trait Monad[M[_]]

object Async {
  // a trampolined type that replaces the Function0 from the original 'Tramp' with Par for async execution

  sealed trait Async[A] {
    def flatMap[B](k: A => Async[B]): Async[B] = FlatMap(this, k)
    def map[B](f: A => B): Async[B] = flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => async
  }

  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a) => Par.unit(a)
    case Suspend(r) => Par.flatMap(r)(a => run(Return(a)))
    case FlatMap(x, f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }
  }
}

object Free {
  // abstract over the effect type; we've been from Function0 -> Async and now Async -> F

  sealed trait Free[F[_], A]
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](a: F[A], f: A => F[B]) extends Free[F, B]

  // then...
  type Tramp[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  // and then...
  //def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f]
  //def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A]

  object ConsoleEffects {
    // creating a effect type that only does ConsoleIO

    // the algebra
    sealed trait ConsoleAlg[A] {
      def toPar: Par[A]
      def toThunk: () => A
    }

    case object ReadLine extends ConsoleAlg[Option[String]] {
      def toPar = Par.lazyUnit(run)
      def toThunk = () => run

      def run: Option[String] =
        try Some(readLine())
        catch { case e: Exception => None }
    }

    case class PrintLine(line: String) extends ConsoleAlg[Unit] {
      def toPar = Par.lazyUnit(println(line))
      def toThunk = () => println(line)
    }

    // the interpreter
    object Console {
      type ConsoleIO[A] = Free[ConsoleAlg, A]

      def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
      def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
    }

    // Given that Free.run requires a Monad[F] we need to provide one for ConsoleAlg, or convert it into one
    trait Translate[F[_], G[_]] {
      def apply[A](f: F[A]): G[A]
    }
    type ~>[F[_], G[_]] = Translate[F, G]
    // Translate/~> define a transformation from F to a G that is a monad and hence can be used in the Free

    val consoleToFunction0 = new (ConsoleAlg ~> Function0) { def apply[A](a: ConsoleAlg[A]) = a.toThunk }
    val consoleToPar = new (ConsoleAlg ~> Par) { def apply[A](a: ConsoleAlg[A]): a.toPar }

    // and then run can be generalized to
    def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
      step(free) match {
        case Return(a) => G.unit(a)
        case Suspend(r) => t(r)
        case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
        case _ => sys.error("Impossible; `step` eliminates these cases")
      }

    def runConsoleFunction0[A](a: Free[ConsoleAlg, A]): () => A = runFree(a)(consoleToFunction0)
    def runConsolePar[A](a: Free[ConsoleAlg, A]): Par[A] = runFree(a)(consoleToPar)

    object PureConsole { // a translation into a Monad that doesn't actually perform effects
      // ah, couldn't be bothered... look at the book
    }
  }
}
