package mymonads


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class MachineState(locked: Boolean, candies: Int, coins: Int)

class CandyMachine(stateAccessor: Input => (MachineState => MachineState) => StateT[SimulationState, Identity, Unit]) {

  def runForInput(input: Input): StateT[SimulationState, Identity, Unit] = {
    input match {
      case Coin => stateAccessor(input) {
        (m: MachineState) =>
          if (m.locked && m.candies > 0) m.copy(locked = false, coins = m.coins + 1)
          else m
      }
      case Turn => stateAccessor(input) {
        (m: MachineState) =>
          if (m.locked) m
          else m.copy(candies = m.candies - 1, locked = true)
      }
    }
  }
}



case class SimulationState(machine: MachineState, recordedInputs: List[Input])

object Simulation {
  val stateM = new StateTOps[SimulationState, Identity](new IdentityOps)

  private def machineStateAccessor(input: Input)(f: MachineState => MachineState): StateT[SimulationState, Identity, Unit] =
    stateM.modify(s => s.copy(
      machine = f(s.machine),
      recordedInputs = s.recordedInputs :+ input)
    )

  private def summaryOfMachine: StateT[SimulationState, Identity, (Int, Int)] =
    stateM.get.map(s => (s.machine.candies, s.machine.coins))

  def runCandyMachine(initialCandyMachine: MachineState, inputs: List[Input]): (Int, Int, MachineState) = {

    val candyMachine = new CandyMachine(machineStateAccessor _)

    val endS = stateM.sequence(inputs.map(candyMachine.runForInput(_)))

    val summaryM = for {
      _ <- endS
      summary <- summaryOfMachine
    } yield summary

    val ((candies, coins), SimulationState(finalMachineState, recordedInputs)) = summaryM.run(SimulationState(initialCandyMachine, Nil)).run
    Predef.assert(recordedInputs != Nil)

    (candies, coins, finalMachineState)
  }
}
