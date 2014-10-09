package mymonads

case class SimulationState(machine: MachineState, recordedInputs: List[Input])

class Simulation {
  val stateM = new StateTOps[SimulationState, Identity](new IdentityOps)

  private def transitionState(f: MachineState => MachineState): StateT[SimulationState, Identity, Unit] =
    stateM.modify(s => s.copy(
      machine = f(s.machine)
    ))

  def runCandyMachine(initialCandyMachine: MachineState, inputs: List[Input]): StateT[SimulationState, Identity, List[Unit]] = {

    val candyMachine = new CandyMachine

    val transitions: List[StateT[SimulationState, Identity, Unit]] = inputs.map{ input =>
      for {
        _ <- stateM.modify{ s => s.copy(recordedInputs = s.recordedInputs :+ input) }
        _ <- transitionState(candyMachine.runForInput(input))
      } yield ()
    }

    val finalState = stateM.sequence(transitions)
    finalState
  }

  def summaryOfMachine(simulation: StateT[SimulationState, Identity, _]): StateT[SimulationState, Identity, (Int, Int)] = {
    for {
      _ <- simulation
      summary <- stateM.get.map(s => (s.machine.candies, s.machine.coins))
    } yield summary
  }

  def initialState(initialMachineState: MachineState): SimulationState = SimulationState(initialMachineState, Nil)
}
