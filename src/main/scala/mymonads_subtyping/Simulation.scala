//package mymonads
//
//case class SimulationState(machine: MachineState, recordedInputs: List[Input])
//
//class Simulation[M[X] <: StateMonad[SimulationState, X, M]](stateOps: StateTOps[SimulationState, M]) {
//
//  private def transitionState(f: MachineState => MachineState): M[Unit] =
//    stateOps.modify(s => s.copy(
//      machine = f(s.machine)
//    ))
//
//  def runCandyMachine(initialCandyMachine: MachineState, inputs: List[Input]): M[List[Unit]] = {
//
//    val candyMachine = new CandyMachine
//
//    val transitions: List[M[Unit]] = inputs.map{ input =>
//      for {
//        _ <- stateOps.modify{ s => s.copy(recordedInputs = s.recordedInputs :+ input) }
//        _ <- transitionState(candyMachine.runForInput(input))
//      } yield ()
//    }
//
//    val finalState = stateOps.sequence(transitions)
//    finalState
//  }
//
//  def summaryOfMachine(simulation: M[_]): M[(Int, Int)] = {
//    for {
//      _ <- simulation
//      summary <- stateOps.get.map(s => (s.machine.candies, s.machine.coins))
//    } yield summary
//  }
//
//  def initialState(initialMachineState: MachineState): SimulationState = SimulationState(initialMachineState, Nil)
//}
