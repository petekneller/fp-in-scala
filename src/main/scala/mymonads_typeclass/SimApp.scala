package mymonads_typeclass

object SimApp {

  def runFor(initialMachine: MachineState, inputs: List[Input]): (Int, Int, MachineState) = {

    val simulation = new Simulation
    val finalState = simulation.runCandyMachine(initialMachine, inputs)

    val summaryM = simulation.summaryOfMachine(finalState)

    val ((candies, coins), SimulationState(finalMachineState, recordedInputs)) = summaryM.run(simulation.initialState(initialMachine))
    Predef.assert(recordedInputs != Nil)

    (candies, coins, finalMachineState)
  }

}
