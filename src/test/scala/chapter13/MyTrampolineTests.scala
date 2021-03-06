package chapter13

import org.scalatest.{Matchers, FunSuite}

class MyTrampolineTests extends FunSuite with Matchers {

  val f1 = (a: Int) => {
    println(s"Depth: ${Thread.currentThread().getStackTrace.toSeq.drop(1).takeWhile(!_.getMethodName.contains("run_$")).map(_.getMethodName).size}")
    a * 2
  }
  val f2 = (a: Int) => f1(a) + 4
  val f3 = (a: Int, b: Int) => f1(a) + f2(b)

  ignore("too much function composition blows the stack") {


    val N = 10 // 1000000
    val f1s = List.fill(N)(f1)
    val fx = f1s.fold(f1)(_ compose _)

    def run_(f: Int => Int, v: Int): Int = f(v)

    // too large an N this will blow the stack
    run_(fx, 2) should equal(4096)
  }

  // A first crack at turning function calls into interpreters
  // ---------------------------------------------------------

  test("just returning values") {
    case class Return[A](v: A)

    def run_[A](t: Return[A]): A = t.v

    // a function that returns a value
    val fx = (a: Int) => Return(a)
    run_(fx(2)) should equal(2)
  }

  test("calling legacy functions") {
    case class Call[A, B](f: A => B, v: A)

    def run_[A, B](t: Call[A, B]): B = t.f(t.v)

    // a function that requests the interpreter to call a legacy function and returns that value
    val fx = (a: Int) => Call(f1, a)
    run_(fx(2)) should equal(4)
  }

  // So that last interpreter can handle function calls only in the tail position.
  // What about where there is something to be done post- function call?

  test("supplying a continuation") {
    case class Call[A, B, C](f: A => B, v: A, cont: B => C)

    def run_[A, B, C](t: Call[A, B, C]): C = t.cont(t.f(t.v))

    val fx = (a: Int) => Call(f1, a, (b: Int) => b + 1)
    run_(fx(2)) should equal(5)
  }

  test("executing a side-effect") {
    case class Return[A](cont: () => A)

    def run_[A](t: Return[A]): A = t.cont()

    val fx = (a: Int) => Return { () => println("fooey"); 2}
    run_(fx(1)) should equal(2)
  }

  test("call a function and then return a value, after executing a side effect") {
    case class Return[A](cont: () => A)
    case class Call[A, B, C](f: A => B, v: A, cont: B => Return[C])

    def run_[A, B, C](f: Call[A, B, C]): C = f.cont(f.f(f.v)).cont()

    val fx = (a: Int) => Call(f1, a, (b: Int) => Return { () => println(b); b + 1})
    run_(fx(2)) should equal(5)
  }

  ignore("arbitrary depth function calls") {
    trait Tramp[+A, +B, C]
    case class Return[A](cont: () => A) extends Tramp[Nothing, Nothing, A]
    case class Call[A, B, C](f: A => B, v: A, cont: B => Tramp[_, _, C]) extends Tramp[A, B, C]

    def run[A, B, C](t: Tramp[A, B, C]): C = {
      t match {
        case Return(cont) => cont()
        case Call(f, v, cont) => run(cont(f(v)))
      }
    }

    def run_[A, B, C](t: Tramp[A, B, C]): C = run(t)

    val t = Call(f1, 2, (b: Int) =>
      Call(f1, b, (c: Int) =>
        Call(f1, c, (d: Int) =>
          Call(f1, d, (e: Int) =>
            Call(f1, e, (f: Int) =>
              Call(f1, f, (g: Int) =>
                Return(() => e + 1)
              )
            )
          )
        )
      )
    )
    run_(t) should equal(33)
  }

  ignore("with tail-call elimination of excess stack frames in the interpreter") {
    trait Tramp[+A, +B, C]
    case class Return[A](cont: () => A) extends Tramp[Nothing, Nothing, A]
    case class Call[A, B, C](f: A => B, v: A, cont: B => Tramp[_, _, C]) extends Tramp[A, B, C]

    import scala.util.control.TailCalls._
    def runtail[A, B, C](t: Tramp[A, B, C]): TailRec[C] = {
      t match {
        case Return(cont) => done(cont())
        case Call(f, v, cont) => tailcall(runtail(cont(f(v))))
      }
    }
    def run_[A, B, C](t: Tramp[A, B, C]): C = runtail(t).result

    val t = Call(f1, 2, (b: Int) =>
      Call(f1, b, (c: Int) =>
        Call(f1, c, (d: Int) =>
          Call(f1, d, (e: Int) =>
            Call(f1, e, (f: Int) =>
              Call(f1, f, (g: Int) =>
                Return(() => e + 1)
              )
            )
          )
        )
      )
    )
    run_(t) should equal(33)
  }

  test("a simpler formulation? removing the legacy function calls") {
    trait Tramp[+A, B]
    case class Return[A](cont: () => A) extends Tramp[Nothing, A]
    case class Call[A, B](v: A, cont: A => Tramp[_, B]) extends Tramp[A, B]

    import scala.util.control.TailCalls._
    def runtail[A, B](t: Tramp[A, B]): TailRec[B] = {
      t match {
        case Return(cont) => done(cont())
        case Call(v, cont) => tailcall(runtail(cont(v)))
      }
    }
    def run_[A, B](t: Tramp[A, B]): B = runtail(t).result


    val t = Call(2, (b: Int) =>
      Call(f1(b), (c: Int) =>
        Call(f1(c), (d: Int) =>
          Call(f1(d), (e: Int) =>
            Call(f1(e), (f: Int) =>
              Call(f1(e), (g: Int) =>
                Return(() => f1(e) + 1)
              )
            )
          )
        )
      )
    )
    run_(t) should equal(33)
  }

  test("a simpler formulation? don't need the extra type parameter") {
    trait Tramp[A]
    case class Return[A](cont: () => A) extends Tramp[A]
    case class Call[A, B](v: A, cont: A => Tramp[B]) extends Tramp[B] // A is only required to ascertain the type of the first Tramp and the type of the fn param are equivalent

    import scala.util.control.TailCalls._
    def runtail[A](t: Tramp[A]): TailRec[A] = {
      t match {
        case Return(cont) => done(cont())
        case Call(v, cont) => tailcall(runtail(cont(v)))
      }
    }
    def run_[A](t: Tramp[A]): A = runtail(t).result


    val t = Call(2, (b: Int) =>
      Call(f1(b), (c: Int) =>
        Call(f1(c), (d: Int) =>
          Call(f1(d), (e: Int) =>
            Call(f1(e), (f: Int) =>
              Call(f1(e), (g: Int) =>
                Return(() => f1(e) + 1)
              )
            )
          )
        )
      )
    )
    run_(t) should equal(33)
  }

  test("the proper implementation - from FP in Scala - but using the names I've used here already") {
    sealed trait Tramp[A]
    case class Return[A](v: A) extends Tramp[A]
    case class Cont[A](cont: () => A) extends Tramp[A]
    case class Call[A, B](a: Tramp[A], f: A => Tramp[B]) extends Tramp[B]

    def flatMap[A, B](t: Tramp[A], f: A => Tramp[B]): Tramp[B] = Call(t, f)

    //@annotation.tailrec
    def run[A, B, C](tramp: Tramp[A]): A = tramp match {
      case Return(a) => a
      case Cont(cont) => cont()
      case Call(x: Tramp[B], f) => x match {
        case Return(a) => run(f(a))
        case Cont(cont) => run(f(cont()))
        case Call(y: Tramp[C], g) => run(Call(y, (c: C) => Call(g(c), f)))
      }
    }
  }

  // Or, an alternate impl of stack safety using Exceptions.
  // Taken from the FP in Scala code, file Throw.scala
  object UsingExceptions {
    /**
      * A version of `TailRec` implemented using exceptions.
      * In the implementation of `flatMap`, rather than calling
      * the function, we throw an exception indicating what
      * function we want to call. A central loop repeatedly tries
      * and catches these exceptions to force the computation.
      */
    trait Throw[+A] {
      import Throw._

      @annotation.tailrec
      final def run: A = this match {
        case Done(a) => a
        case More(thunk) => force(thunk).run
      }
    }

    object Throw {

      /* Exception indicating that the central loop should call `f(a)`. */
      case class Call[A,+B] private[Throw] (a: A, f: A => B) extends Exception {
        override def fillInStackTrace = this
      }

      case class Done[+A](a: A) extends Throw[A]
      case class More[+A](thunk: () => Throw[A]) extends Throw[A]

      /* Defer evaluation of `f(a)` to the central evaluation loop. */
      def defer[A,B](a: A)(f: A => B): B =
        throw new Call(a, f)

      /* Central evaluation loop. */
      def ap[A,B](a: A)(f: A => B): B = {
        var ai: Any = a
        var fi: Any => Any = f.asInstanceOf[Any => Any]
        while (true) {
          try return fi(ai).asInstanceOf[B]
          catch { case Call(a2,f2) => ai = a2; fi = f2; }
        }
        return null.asInstanceOf[B] // unreachable
      }

      /* Convenience function for forcing a thunk. */
      def force[A](f: () => A): A =
        ap(f)(f => f())

      def more[A](a: => Throw[A]): Throw[A] = More(() => a)

      /* `Throw` forms a `Monad`.

      def unit[A](a: => A): Throw[A] = more(Done(a))

      def flatMap[A,B](a: Throw[A])(f: A => Throw[B]): Throw[B] =
        a match {
          case Done(a) => f(a)
          case More(thunk) =>
            try thunk() flatMap f
            catch { case Call(a0,g) => more {
              defer(a0)(g.asInstanceOf[Any => Throw[A]].
                andThen(_ flatMap f))
            }}
        }

       */
    }}

}
