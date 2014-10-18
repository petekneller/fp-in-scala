package scalazmonads

import org.scalatest.FunSuite
import scalaz._
import scalaz.std.list.listInstance
import scalaz.Writer._

class CandyMachineTest extends FunSuite {

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)
  
  case class Simulation(machine: Machine, recordedInputs: List[Input])

  type WriterTState[A] = WriterT[({type l[X] = State[Simulation, X]})#l, List[Input], A]
  
  object Simulation {
    def transitionMachine(input: Input)(f: Machine => Machine): WriterTState[Unit] =
      MonadState[({type l[S, A] = WriterT[({type l[B] = State[S, B]})#l, List[Input], A]})#l, Simulation].modify(s => s.copy(
        machine = f(s.machine),
        recordedInputs = s.recordedInputs :+ input)
      )

    def summarizeMachine: WriterTState[(Int, Int)] =
      MonadState[({type l[S, A] = WriterT[({type l[B] = State[S, B]})#l, List[Input], A]})#l, Simulation].get.map(s => (s.machine.candies, s.machine.coins))
  }

  def simulateMachine(inputs: List[Input]): WriterTState[(Int, Int)] = {
    def singleTransition(input: Input): WriterTState[Unit] = {
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

    val endS = Traverse[List].sequence[WriterTState, Unit](inputs.map(singleTransition))

    for {
      _ <- endS
      summary <- Simulation.summarizeMachine
    } yield summary
  }

  def runSimFor(inputs: List[Input], initialMachine: Machine): (Int, Int, Machine) = {
    val simulation = simulateMachine(inputs)
    val (Simulation(endMachine, _), (recordedInputs, (candies, coins))) = simulation.run(Simulation(initialMachine, Nil))
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
