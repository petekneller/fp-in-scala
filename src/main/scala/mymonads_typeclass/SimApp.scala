package mymonads_typeclass

object SimApp {

  def runFor(initialMachine: MachineState, inputs: List[Input]): (Int, Int, MachineState) = {

    val identityOps = new IdentityOps
    val stateOps = new StateTOps[SimulationState, Identity](identityOps)
    val writerOps = new WriterTOps[Input, ({ type L[A] = StateT[SimulationState, Identity, A]})#L](stateOps)
    val writerStateMonad = WriterTOps.toStateMonad[SimulationState, Input, ({ type L[A] = StateT[SimulationState, Identity, A]})#L](writerOps, stateOps)

    val simulation = new Simulation[({ type G[B] = WriterT[Input, ({ type L[A] = StateT[SimulationState, Identity, A]})#L, B]})#G](writerStateMonad, writerOps)

    val finalState = simulation.runCandyMachine(initialMachine, inputs)
    val summaryM = simulation.summaryOfMachine(finalState)

    val (SimulationState(finalMachineState), (recordedInputs2, (candies, coins))) = summaryM.run(List.empty).run(simulation.initialState(initialMachine)).run
    Predef.assert(recordedInputs2 != Nil)

    (candies, coins, finalMachineState)
  }

}
