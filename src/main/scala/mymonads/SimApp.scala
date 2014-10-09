package mymonads

object SimApp {

  val monadOps = new StateTOps[SimulationState, Identity](new IdentityOps)

  def runFor(initialMachine: MachineState, inputs: List[Input]): (Int, Int, MachineState) = {

    val simulation = new Simulation[Identity](monadOps)
    val finalState = simulation.runCandyMachine(initialMachine, inputs)

    val summaryM: StateT[SimulationState, Identity, (Int, Int)] = simulation.summaryOfMachine(finalState)

    val ((candies, coins), SimulationState(finalMachineState, recordedInputs)) = summaryM.run(simulation.initialState(initialMachine)).run
    Predef.assert(recordedInputs != Nil)

    (candies, coins, finalMachineState)
  }

}
