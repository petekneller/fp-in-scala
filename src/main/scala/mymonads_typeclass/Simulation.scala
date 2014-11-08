package mymonads_typeclass

case class SimulationState(machine: MachineState, recordedInputs: List[Input])

class Simulation[T[_]](stateMonad: StateMonad[SimulationState, T]) {

  import stateMonad.monadImplicit

  private def transitionState(f: MachineState => MachineState): T[Unit] =
    stateMonad.modify(s => s.copy(
      machine = f(s.machine)
    ))

  def runCandyMachine(initialCandyMachine: MachineState, inputs: List[Input]): T[List[Unit]] = {

    val candyMachine = new CandyMachine

    val transitions: List[T[Unit]] = inputs.map{ input =>
      for {
        _ <- stateMonad.modify{ s => s.copy(recordedInputs = s.recordedInputs :+ input) }
        _ <- transitionState(candyMachine.runForInput(input))
      } yield ()
    }

    val finalState = stateMonad.sequence(transitions)
    finalState
  }

  def summaryOfMachine(simulation: T[_]): T[(Int, Int)] = {
    for {
      _ <- simulation
      summary <- stateMonad.get.map(s => (s.machine.candies, s.machine.coins))
    } yield summary
  }

  def initialState(initialMachineState: MachineState): SimulationState = SimulationState(initialMachineState, Nil)
}
