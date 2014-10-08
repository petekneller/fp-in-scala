package mymonads

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State {
    s =>
      val (a, s2) = run(s)
      val b = f(a)
      (b, s2)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State {
    s =>
      val (a, s2) = run(s)
      f(a).run(s2)
  }
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[A](s: A): State[A, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    oldS <- State.get[S]
    newS = f(oldS)
    _ <- State.set(newS)
  } yield ()

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = State {
    (s: S) =>
      val (a, s2) = sa.run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
  }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldLeft(unit[S, List[A]](Nil))((acc: State[S, List[A]], f: State[S, A]) => map2(acc, f)(_ :+ _))
  }
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

case class Simulation(machine: Machine, recordedInputs: List[Input])

object Simulation {
  def transitionMachine(input: Input)(f: Machine => Machine): State[Simulation, Unit] =
    State.modify(s => s.copy(
      machine = f(s.machine),
      recordedInputs = s.recordedInputs :+ input)
    )

  def summarizeMachine: State[Simulation, (Int, Int)] =
    State.get[Simulation].map(s => (s.machine.candies, s.machine.coins))
}

object CandyMachine {
  def simulateMachine(inputs: List[Input]): State[Simulation, (Int, Int)] = {

    def singleTransition(input: Input): State[Simulation, Unit] = {
      input match {
        case Coin => Simulation.transitionMachine(input) {
          (m: Machine) =>
            if (m.locked && m.candies > 0) m.copy(locked = false, coins = m.coins + 1)
            else m
        }
        case Turn => Simulation.transitionMachine(input) {
          (m: Machine) =>
            if (m.locked) m
            else m.copy(candies = m.candies - 1, locked = true)
        }
      }
    }

    val endS = State.sequence(inputs.map(singleTransition))

    for {
      _ <- endS
      summary <- Simulation.summarizeMachine
    } yield summary
  }
}
