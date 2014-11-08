package mymonads_typeclass

object SimApp {

  def runFor(initialMachine: MachineState, inputs: List[Input]): (Int, Int, MachineState) = {

    type M1[X] = Identity[X]
    type M2[X] = StateT[SimulationState, M1, X]
    type M3[X] = WriterT[Input, M2, X]

    val identityOps = new IdentityOps
    val stateOps = new StateTOps[SimulationState, M1](identityOps)
    val writerOps = new WriterTOps[Input, M2](stateOps)
    val writerStateMonad = WriterTOps.toStateMonad[SimulationState, Input, M2](writerOps, stateOps)

    val simulation = new Simulation[M3](writerStateMonad, writerOps)

    val finalState = simulation.runCandyMachine(initialMachine, inputs)
    val summaryM = simulation.summaryOfMachine(finalState)

    val (SimulationState(finalMachineState), (recordedInputs2, (candies, coins))) = summaryM.run(List.empty).run(simulation.initialState(initialMachine)).run
    Predef.assert(recordedInputs2 != Nil)

    (candies, coins, finalMachineState)
  }

}
