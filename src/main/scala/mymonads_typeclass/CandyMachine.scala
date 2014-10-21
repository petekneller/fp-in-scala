package mymonads_typeclass

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class MachineState(locked: Boolean, candies: Int, coins: Int)

class CandyMachine {

  def runForInput(input: Input): (MachineState => MachineState) = {
    input match {
      case Coin => {
        (m: MachineState) =>
          if (m.locked && m.candies > 0) m.copy(locked = false, coins = m.coins + 1)
          else m
      }
      case Turn => {
        (m: MachineState) =>
          if (m.locked) m
          else m.copy(candies = m.candies - 1, locked = true)
      }
    }
  }
}
