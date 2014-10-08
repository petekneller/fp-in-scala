package chapter6

import org.scalatest.FunSuite

class CandyMachineTest extends FunSuite {

  case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B): State[S, B] = State {
      s =>
        val (a, s2) = run(s)
        val b = f(a)
        (b, s2)
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State {
      s =>
        val (a, s2) = run(s)
        f(a).run(s2)
    }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[A](s: A): State[A, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
      oldS <- State.get[S]
      newS = f(oldS)
      _ <- State.set(newS)
    } yield ()

    def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = State{
      (s: S) =>
        val (a, s2) = sa.run(s)
        val (b, s3) = sb.run(s2)
        (f(a, b), s3)
    }

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
      fs.foldLeft(unit[S, List[A]](Nil))((acc: State[S, List[A]], f: State[S, A]) => map2(acc, f)(_ :+ _))
    }
  }

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)
  
  case class Simulation(machine: Machine, recordedInputs: List[Input])
  
  object Simulation {
    def transitionMachine(input: Input)(f: Machine => Machine): State[Simulation, Unit] =
      State.modify(s => s.copy(
        machine = f(s.machine),
        recordedInputs = s.recordedInputs :+ input)
      )

    def summarizeMachine: State[Simulation, (Int, Int)] =
      State.get[Simulation].map(s => (s.machine.candies, s.machine.coins))
  }

  def simulateMachine(inputs: List[Input]): State[Simulation, (Int, Int)] = {
    def singleTransition(input: Input): State[Simulation, Unit] = {
      input match {
        case Coin => Simulation.transitionMachine(input) {
          (m: Machine) =>
            if (m.locked && m.candies > 0) m.copy(locked = false, coins = m.coins + 1)
            else m
        }
        case Turn => Simulation.transitionMachine(input) {
          (m: Machine) =>
            if (m.locked) m
            else m.copy(candies = m.candies - 1, locked = true)
        }
      }
    }

    val endS = State.sequence(inputs.map(singleTransition))

    for {
      _ <- endS
      summary <- Simulation.summarizeMachine
    } yield summary
  }

  def runSimFor(inputs: List[Input], initialMachine: Machine): (Int, Int, Machine) = {
    val simulation = simulateMachine(inputs)
    val ((candies, coins), Simulation(endMachine, recordedInputs)) = simulation.run(Simulation(initialMachine, Nil))
    Predef.assert(recordedInputs != Nil)
    (candies, coins, endMachine)
  }

  test("locked -> {coin inserted} -> unlocked") {

    val (candies, coins, endMachine) = runSimFor(List(Coin), Machine(true, 1, 0))

    assert(endMachine.locked === false)
    assert(coins === 1)
  }

  test("unlocked -> {coin inserted} -> unlocked (doesn't keep coin)") {

    val (candies, coins, machine) = runSimFor(List(Coin), Machine(false, 1, 0))
    assert(machine.locked === false)
    assert(coins === 0)
  }

  test("locked -> {knob turned} -> locked") {

    val (candies, coins, machine) = runSimFor(List(Turn), Machine(true, 1, 0))

    assert(machine.locked === true)
    assert(candies === 1)
    assert(coins === 0)
  }

  test("* -> {* if out of candy} -> does nothing") {

    val (candies, coins, machine) = runSimFor(List(Coin), Machine(true, 0, 0))

    assert(machine.locked === true)
    assert(candies === 0)
    assert(coins === 0)
  }

  test("unlocked -> {knob turned} -> locked (and dispenses candy)") {

    val (candies, coins, machine) = runSimFor(List(Turn), Machine(false, 1, 1))

    assert(machine.locked === true)
    assert(candies === 0)
    assert(coins === 1)
  }

  test("everything") {

    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val (candies, coins, _) = runSimFor(inputs, Machine(true, 5, 10))

    assert(candies === 1)
    assert(coins === 14)
  }

}
