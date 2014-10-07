package chapter6

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

class PureRandomNumberTests extends FunSuite with PropertyChecks with Matchers {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (Math.abs(i), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble / Int.MaxValue, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = rng2.nextInt
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    count match {
      case 0 => (Nil, rng)
      case _ =>
        val (i, rng2) = rng.nextInt
        val (is, rng3) = ints(count-1)(rng2)
        (i :: is, rng3)
    }
  }


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = (rng) => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    (rng: RNG) =>
      val (a, rng2) = s(rng)
      val b = f(a)
      (b, rng2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    (rng: RNG) =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft(unit(Nil: List[A]))((acc, f) => map2(acc, f)(_ :+ _))
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    val fs = List.fill(count)(int)
    sequence(fs)(rng)
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    (rng: RNG) =>
      val (a, rng2) = f(rng)
      g(a)(rng2)
  }



  test("exercise 6.1") {
    forAll{ (l: Long) => (nonNegativeInt(SimpleRNG(l))._1) should be >= 0 }
  }

  test("exercise 6.2") {
    forAll{ (l: Long) =>
      val d = double(SimpleRNG(l))._1
      d should (be >= 0.0 and be < 1.0)
    }
  }

  test("exercise 6.5") {
    def double2(rng: RNG): (Double, RNG) = {
      map(nonNegativeInt _)(_.toDouble / Int.MaxValue)(rng)
    }

    forAll{ (l: Long) =>
      val d = double2(SimpleRNG(l))._1
      d should (be >= 0.0 and be < 1.0)
    }
  }

  test("exercise 6.6") {

    forAll{ (l: Long) =>
      val (r, _) = map2(nonNegativeInt _, int)((_, _))(SimpleRNG(l))
      r._1 should (be >= 0)
    }
  }

  test("exercise 6.7") {

    forAll{ (l: Long) =>
      val fs = List(nonNegativeInt _, int, unit(3))
      val (rs, _) = sequence(fs)(SimpleRNG(l))
      rs should have length (3)
      rs(0) should (be >= 0)
      rs(2) should be (3)
    }
  }

  test("exercise 6.8") {
    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt _){ i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }
    }

    forAll{ (l: Long) =>
      val (i, _) = nonNegativeLessThan(3)(SimpleRNG(l))
      i should be >= 0
      i should be < 3
    }
  }

  test("exercise 6.9") {

    def mapb[A, B](s: Rand[A])(f: A => B): Rand[B] = {
      flatMap(s)(a => unit(f(a)))
    }

    def map2b[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))
    }

    forAll{ (l: Long) =>
      val (r, _) = map2b(nonNegativeInt _, int)((_, _))(SimpleRNG(l))
      r._1 should (be >= 0)
    }
  }

}