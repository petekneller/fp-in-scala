package mymonads


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



case class SimulationState(machine: MachineState, recordedInputs: List[Input])

object Simulation {
  val stateM = new StateTOps[SimulationState, Identity](new IdentityOps)

  private def transitionState(f: MachineState => MachineState): StateT[SimulationState, Identity, Unit] =
    stateM.modify(s => s.copy(
      machine = f(s.machine)
    ))

  private def summaryOfMachine: StateT[SimulationState, Identity, (Int, Int)] =
    stateM.get.map(s => (s.machine.candies, s.machine.coins))

  def runCandyMachine(initialCandyMachine: MachineState, inputs: List[Input]): (Int, Int, MachineState) = {

    val candyMachine = new CandyMachine

    val transitions: List[StateT[SimulationState, Identity, Unit]] = inputs.map{ input =>
      for {
        _ <- stateM.modify{ s => s.copy(recordedInputs = s.recordedInputs :+ input) }
        _ <- transitionState(candyMachine.runForInput(input))
      } yield ()
    }

    val transitionsM = stateM.sequence(transitions)

    val summaryM = for {
      _ <- transitionsM
      summary <- summaryOfMachine
    } yield summary

    val ((candies, coins), SimulationState(finalMachineState, recordedInputs)) = summaryM.run(SimulationState(initialCandyMachine, Nil)).run
    Predef.assert(recordedInputs != Nil)

    (candies, coins, finalMachineState)
  }
}
