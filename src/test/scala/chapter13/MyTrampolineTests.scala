package chapter13

import org.scalatest.{Matchers, FunSuite}

class MyTrampolineTests extends FunSuite with Matchers {

  val f1 = (a: Int) => {
    println(s"Depth: ${Thread.currentThread().getStackTrace.toSeq.drop(1).takeWhile(!_.getMethodName.contains("run_$")).map(_.getMethodName).size}")
    a * 2
  }
  val f2 = (a: Int) => f1(a) + 4
  val f3 = (a: Int, b: Int) => f1(a) + f2(b)

  test("too much function composition blows the stack") {


    val N = 10 // 1000000
    val f1s = List.fill(N)(f1)
    val fx = f1s.fold(f1)(_ compose _)

    def run_(f: Int => Int, v: Int): Int = f(v)

    // too large an N this will blow the stack
    run_(fx, 2) should equal(2)
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

  test("arbitrary depth function calls") {
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

  test("with tail-call elimination of excess stack frames in the interpreter") {
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

  // TODO
  //   what's the proper formulation of trampolining?

}
