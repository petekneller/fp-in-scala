package mymonads_typeclass

object SimApp {

  def runFor(initialMachine: MachineState, inputs: List[Input]): (Int, Int, MachineState) = {

//    /* StateT[WriterT[Identity]]
    type M1[X] = Identity[X]
    type M2[X] = WriterT[Input, M1, X]
    type M3[X] = StateT[SimulationState, M2, X]

    val identityOps = new IdentityOps
    val writerOps = new WriterTOps[Input, M1](identityOps)
    val stateOps = new StateTOps[SimulationState, M2](writerOps)

    val stateWriterMonad = WriterTOps.toWriterMonad[Input, M2, M3](stateOps, writerOps)

    val simulation = new Simulation[M3](stateOps, stateWriterMonad)
//    */

    /*  WriterT[StateT[Identity]]
    type M1[X] = Identity[X]
    type M2[X] = StateT[SimulationState, M1, X]
    type M3[X] = WriterT[Input, M2, X]

    val identityOps = new IdentityOps
    val stateOps = new StateTOps[SimulationState, M1](identityOps)
    val writerOps = new WriterTOps[Input, M2](stateOps)

    val writerStateMonad = StateTOps.toStateMonad[SimulationState, M2, M3](writerOps, stateOps)

    val simulation = new Simulation[M3](writerStateMonad, writerOps)
    */

    // common
    val finalState = simulation.runCandyMachine(initialMachine, inputs)
    val summaryM = simulation.summaryOfMachine(finalState)


//    /*   StateT[WriterT[Identity]]
    val (recordedInputs, (SimulationState(finalMachineState), (candies, coins))) = summaryM.run(simulation.initialState(initialMachine)).run(List.empty).run
//    */

    /*   WriterT[StateT[Identity]]
    val (SimulationState(finalMachineState), (recordedInputs, (candies, coins))) = summaryM.run(List.empty).run(simulation.initialState(initialMachine)).run
    */

    // common
    Predef.assert(recordedInputs != Nil)
    (candies, coins, finalMachineState)
  }

}
