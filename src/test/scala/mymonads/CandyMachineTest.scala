package mymonads

import org.scalatest.FunSuite

class CandyMachineTest extends FunSuite {

  def runSimFor(inputs: List[Input], initialMachine: Machine): (Int, Int, Machine) = {
    val simulation = CandyMachine.simulateMachine(inputs)
    val ((candies, coins), Simulation(endMachine, recordedInputs)) =
        simulation.run(Simulation(initialMachine, Nil)).run
    
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
