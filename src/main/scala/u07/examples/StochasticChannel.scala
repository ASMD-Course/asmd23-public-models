package u07.examples

import u07.modelling.CTMC

object StochasticChannel:
  enum State:
    case IDLE, SEND, DONE, FAIL;

  export State.*
  export u07.modelling.CTMCSimulation.*

  def stocChannel: CTMC[State] = CTMC.ofTransitions(
    Transition(IDLE,1.0 --> SEND),
    Transition(SEND,100_000.0 --> SEND),
    Transition(SEND,200_000.0 --> DONE),
    Transition(SEND,100_000.0 --> FAIL),
    Transition(FAIL,100_000.0 --> IDLE),
    Transition(DONE,1.0 --> DONE)
  )

@main def mainStochasticChannel() =  // example run
  import StochasticChannel.*
  State.values.foreach(s => println(s"$s,${stocChannel.transitions(s)}"))
