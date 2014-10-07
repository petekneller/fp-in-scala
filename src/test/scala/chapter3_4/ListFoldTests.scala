package chapter3_4

import org.scalatest.FunSuite
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

  def flatten[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil: List[A])((subList, out) => foldRight(subList, out)((a, out) => Cons(a, out)))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((a, bs) => Cons(f(a), bs))

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  val testList = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
  val revTestList = Cons(4, Cons(3, Cons(2, Cons(1, Nil))))
  val twoTestLists = Cons(1, Cons(2, Cons(3, Cons(4, Cons(1, Cons(2, Cons(3, Cons(4, Nil))))))))

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

  test("exercise 3.14") {
    def append2[A](as: List[A], a: A): List[A] = foldRight(as, Cons(a, Nil))(Cons.apply _)

    assert(append2(testList, 5) === Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil))))))
  }

  test("exercise 3.15") {

    assert(flatten(Cons(testList, Cons(testList, Nil))) === twoTestLists)
  }

  test("exercise 3.20") {

    val as = Cons(1, Cons(2, Nil))
    assert(flatMap(as)(a => testList) === twoTestLists)
  }

  test("exercise 3.21") {

    def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)((a) => if (f(a)) Cons(a, Nil) else Nil)

    assert(filter2(testList)(_ % 2 == 0) === Cons(2, Cons(4, Nil)))
  }

  ignore("exercise 3.24") {

    def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = {
//      val potentiallyMatching = dropWhile(as, _ != head(sub))
//      val matching = every(zipWith(potentiallyMatching, sub, _ == _))
//      if (matching)
//        true
//      else
//        hasSubsequence(tail(potentiallyMatching), sub)
      false // too lazy to implement all the util fns
    }

    assert(hasSubsequence(testList, Cons(2, Cons(3, Nil))) === true)
    assert(hasSubsequence(testList, Cons(3, Cons(2, Nil))) === false)
  }


}
