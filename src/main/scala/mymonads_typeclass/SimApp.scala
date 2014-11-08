package mymonads_typeclass

object SimApp {

  def runFor(initialMachine: MachineState, inputs: List[Input]): (Int, Int, MachineState) = {

    val stateOps = new StateTOps[SimulationState, Identity](new IdentityOps)

    val simulation = new Simulation[({ type L[S, A] = StateT[S, Identity, A]})#L](stateOps)

    val finalState = simulation.runCandyMachine(initialMachine, inputs)
    val summaryM = simulation.summaryOfMachine(finalState)

    val (SimulationState(finalMachineState, recordedInputs), (candies, coins)) = summaryM.run(simulation.initialState(initialMachine)).run
    Predef.assert(recordedInputs != Nil)

    (candies, coins, finalMachineState)
  }

}
