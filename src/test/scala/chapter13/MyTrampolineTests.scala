package chapter13

import org.scalatest.{Matchers, FunSuite}

class MyTrampolineTests extends FunSuite with Matchers {

//  case class Fn[A](a: A => A) {
//    def flatMap(f: A => Fn[A]): Fn[A] = Fn(f(a))
//    def map(f: A => A): Fn[A] = Fn(f(a))
//  }

  test("too much function composition blows the stack") {

    val f1 = (a: Int) => a
    val f1s = List.fill(1000000)(f1)
    val f2 = f1s.fold(f1)(_ compose _)
//    f2(2) should equal(2)

//    val a = Fn((a: Int) => a)
//    val f3s = List.fill(1000)(a)
//    val f4 = f3s.foldLeft[Fn[List[Int => Int]]](Fn(List.empty[Int => Int])){
//      (acc, i) =>
//        for {
//          as <- acc
//          a <- i
//        } yield as compose a
//    }
//    f4()
  }

  test("function composition can be trampolined") {

    trait Tramp[A]
    case class Return[A](a: A) extends Tramp[A]
    case class Call[A, B](a: A, f: A => Tramp[B]) extends Tramp[A]

    val f = (a: Int) => Return(a)
    def compose[A, B, C](f1: A => B, f2: B => C): A => C = (a: Int) => f2(f1(a))

    f(2).a should equal(2)
  }

}
