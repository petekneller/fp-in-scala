package chapter13

import org.scalatest.{Matchers, FunSuite}

class MyTrampolineTests extends FunSuite with Matchers {

  val f1 = (a: Int) => a * 2
  val f2 = (a: Int) => f1(a) + 4
  val f3 = (a: Int, b: Int) => f1(a) + f2(b)

  test("too much function composition blows the stack") {


    val f1s = List.fill(1000000)(f1)
    val fx = f1s.fold(f1)(_ compose _)

    //this will blow the stack
//    fx(2) should equal(2)
  }

  // A first crack at turning function calls into interpreters
  // ---------------------------------------------------------

  test("just returning values") {
    case class Return[A](v: A)

    def run[A](t: Return[A]): A = t.v

    // a function that returns a value
    val fx = (a: Int) => Return(a)
    run(fx(2)) should equal(2)
  }

  test("calling legacy functions") {
    case class Call[A, B](f: A => B, v: A)

    def run[A, B](t: Call[A, B]): B = t.f(t.v)

    // a function that requests the interpreter to call a legacy function and returns that value
    val fx = (a: Int) => Call(f1, a)
    run(fx(2)) should equal(4)
  }

  // So that last interpreter can handle function calls only in the tail position.
  // What about where there is something to be done post- function call?

  test("supplying a continuation") {
    case class Call[A, B, C](f: A => B, v: A, cont: B => C)

    def run[A, B, C](t: Call[A, B, C]): C = t.cont(t.f(t.v))

    val fx = (a: Int) => Call(f1, a, (b: Int) => b + 1)
    run(fx(2)) should equal(5)
  }

  test("executing a side-effect") {
    case class Return[A](cont: () => A)

    def run[A](t: Return[A]): A = t.cont()

    val fx = (a: Int) => Return{ () => println("fooey"); 2 }
    run(fx(1)) should equal(2)
  }

  test("call a function and then return a value, after executing a side effect") {
    case class Return[A](cont: () => A)
    case class Call[A, B, C](f: A => B, v: A, cont: B => Return[C])

    def run[A, B, C](f: Call[A, B, C]): C = f.cont(f.f(f.v)).cont()

    val fx = (a: Int) => Call(f1, a, (b: Int) => Return{ () => println(b); b + 1 })
    run(fx(2)) should equal(5)
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

    val t = Call(f1, 2, (b: Int) => Call(f2, b, (c: Int) => Return(() => c + 1)))
    run(t) should equal(13)
  }

}
