package mymonads


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

case class Simulation(machine: Machine, recordedInputs: List[Input])

object CandyMachine {
  val stateM = new StateTOps[Simulation]

  def transitionMachine(input: Input)(f: Machine => Machine): StateT[Simulation, Unit] =
    stateM.modify(s => s.copy(
      machine = f(s.machine),
      recordedInputs = s.recordedInputs :+ input)
    )

  def summarizeMachine: StateT[Simulation, (Int, Int)] =
    stateM.get.map(s => (s.machine.candies, s.machine.coins))

  def simulateMachine(inputs: List[Input]): StateT[Simulation, (Int, Int)] = {

    def singleTransition(input: Input): StateT[Simulation, Unit] = {
      input match {
        case Coin => transitionMachine(input) {
          (m: Machine) =>
            if (m.locked && m.candies > 0) m.copy(locked = false, coins = m.coins + 1)
            else m
        }
        case Turn => transitionMachine(input) {
          (m: Machine) =>
            if (m.locked) m
            else m.copy(candies = m.candies - 1, locked = true)
        }
      }
    }

    val endS = stateM.sequence(inputs.map(singleTransition))

    for {
      _ <- endS
      summary <- summarizeMachine
    } yield summary
  }
}
