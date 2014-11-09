package mymonads_typeclass

object SimApp {

  def runFor(initialMachine: MachineState, inputs: List[Input]): (Int, Int, MachineState) = {

    type M1[X] = Identity[X]
    type M2[X] = WriterT[Input, M1, X]
    type M3[X] = StateT[SimulationState, M2, X]

    val identityOps = new IdentityOps
    val writerOps = new WriterTOps[Input, M1](identityOps)
    val stateOps = new StateTOps[SimulationState, M2](writerOps)

    val stateWriterMonad = StateTOps.toWriterMonad[Input, SimulationState, M2](stateOps, writerOps)

    val simulation = new Simulation[M3](stateOps, stateWriterMonad)

    val finalState = simulation.runCandyMachine(initialMachine, inputs)
    val summaryM = simulation.summaryOfMachine(finalState)

    val (recordedInputs2, (SimulationState(finalMachineState), (candies, coins))) = summaryM.run(simulation.initialState(initialMachine)).run(List.empty).run
    Predef.assert(recordedInputs2 != Nil)

    (candies, coins, finalMachineState)
  }

}
