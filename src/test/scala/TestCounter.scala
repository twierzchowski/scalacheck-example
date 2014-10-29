import org.scalacheck._
import org.scalacheck.Prop._
import Gen._


class Counter {
      private var n = 0
      def inc = n += 1
      def dec = n -= 1
      //def dec = if (n>10) n-=2 else  n-=1
      def get = n
      def reset = n = 0
    }


object CounterSpecification extends Commands {

      // This is our system under test. All commands run against this instance.
      val counter = new Counter

      // This is our state type that encodes the abstract state. The abstract state
      // should model all the features we need from the real state, the system
      // under test. We should leave out all details that aren't needed for
      // specifying our pre- and postconditions. The state type must be called
      // State and be immutable.
      case class State(n: Int)

      // initialState should reset the system under test to a well defined
      // initial state, and return the abstract version of that state.
      def initialState() = {
        counter.reset
        State(counter.get)
      }

      // We define our commands as subtypes of the traits Command or SetCommand.
      // Each command must have a run method and a method that returns the new
      // abstract state, as it should look after the command has been run.
      // A command can also define a precondition that states how the current
      // abstract state must look if the command should be allowed to run.
      // Finally, we can also define a postcondition which verifies that the
      // system under test is in a correct state after the command exectution.

      case object Inc extends Command {
        def run(s: State) = counter.inc
        def nextState(s: State) = State(s.n + 1)

        // if we want to define a precondition, we add a function that
        // takes the current abstract state as parameter and returns a boolean
        // that says if the precondition is fulfilled or not. In this case, we
        // have no precondition so we just let the function return true. Obviously,
        // we could have skipped adding the precondition at all.
        preConditions += (s => true)
      }

      case object Dec extends Command {
        def run(s: State) = counter.dec
        def nextState(s: State) = State(s.n - 1)
      }

      case object Get extends Command {
        def run(s: State) = counter.get
        def nextState(s: State) = s

        // when we define a postcondition, we add a function that
        // takes three parameters, s0, s1 and r. s0 is the abstract state before
        // the command was run, s1 is the state after the command was run
        // and r is the result from the command's run method. The
        // postcondition function should return a Boolean (or
        // a Prop instance) that says if the condition holds or not.
        postConditions += {
          case (s0, s1, r:Int) => r == s0.n
          case _ => false
        }
      }

      // This is our command generator. Given an abstract state, the generator
      // should return a command that is allowed to run in that state. Note that
      // it is still neccessary to define preconditions on the commands if there
      // are any. The generator is just giving a hint of which commands that are
      // suitable for a given state, the preconditions will still be checked before
      // a command runs. Sometimes you maybe want to adjust the distribution of
      // your command generator according to the state, or do other calculations
      // based on the state.
      def genCommand(s: State): Gen[Command] = Gen.oneOf(Inc, Dec, Get)

    }

object CounterProperties extends Properties("TestCounter"){
        property("stateful") = CounterSpecification
}


    object TestCounterMain {
        def main(args: Array[String]) {
                CounterProperties.check
        }
}

