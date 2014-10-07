package chapter3_4

import org.scalatest.FunSuite
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.annotation.tailrec

class ListFoldTests extends FunSuite {

  trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](a: A, l: List[A]) extends List[A]

  def foldRight[A, B](as: => List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  @tailrec
  final def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def reverse[A](as: List[A]): List[A] = {
    @tailrec def fold0(as: List[A], sa: List[A]): List[A] = {
      as match {
        case Nil => sa
        case Cons(x, xs) => fold0(xs, Cons(x, sa))
      }
    }

    fold0(as, Nil)
  }

  def append[A](as: List[A], a: A): List[A] = {
    as match {
      case Nil => Cons(a, Nil)
      case Cons(x, xs) => Cons(x, append(xs, a))
    }
  }

  def flip[A, B, C](f: (A, B) => C): (B, A) => C = (b, a) => f(a, b)

  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  val testList = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
  val revTestList = Cons(4, Cons(3, Cons(2, Cons(1, Nil))))

  test("reverse") {

    assert(reverse(testList) === revTestList)

    def naeveReverse[A](as: List[A]): List[A] = {
      as match {
        case Nil => Nil
        case Cons(x, xs) => append(naeveReverse(xs), x)
      }
    }

    assert(naeveReverse(testList) === revTestList)

  }

  test("exercise 3.8") {

    val b = foldRight(testList, Nil: List[Int])(Cons(_, _))
    assert(testList === b)
  }

  test("exercise 3.10") {

    val b = foldLeft(testList, Nil: List[Int])((b, a) => Cons(a, b))
    assert(b === revTestList)
  }

  test("exercise 3.11") {

    assert(sumLeft(testList) === 10)
  }

  test("exercise 3.12") {

    val b = foldLeft(testList, Nil: List[Int])((b, a) => Cons(a, b))
    assert(b === revTestList)
  }

  test("exercise 3.13") {

    def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      def fold0[A, B](as: List[A], z: B, f: (A, B) => B, rem: List[A]): B = {
        as match {
          case Nil => foldLeft(rem, z)((b, a) => f(a, b))
          case Cons(x, xs) => fold0(xs, z, f, Cons(x, rem))
        }
      }

      fold0(as, z, f, Nil)
    }

    assert(foldRight2(testList, Nil: List[Int])(Cons.apply _) === testList)

    def foldRight3[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      foldLeft(reverse(as), z)(flip(f))
    }

    assert(foldRight3(testList, Nil: List[Int])(Cons.apply _) === testList)

  }


}
