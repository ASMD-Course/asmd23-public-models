package scala.lab.u08

object Experiments extends App:

  export CTMCExperimentExtension.*
  export u08.examples.StochasticChannel.*

  def eventuallyReachDone() =

    val reachDone: Property[State] = stocChannel.eventually(_ == DONE)
    val probReachDone = stocChannel.experiment(
      runs = 1000,
      prop = reachDone,
      s0 = IDLE,
      timeBound = 5.0
    )
    println(s"Probability of eventually reaching DONE from IDLE within 10 time units: $probReachDone")

  def isNotDoneUntilReachFail() =
    val isFail: Property[State] = stocChannel.now(_ == FAIL)
    val isDone: Property[State] = stocChannel.now(_ == DONE)
    val neverReachDoneUntilFail: Property[State] = !isDone U isFail
    val probReachFail = stocChannel.experiment(
      runs = 1000,
      prop = neverReachDoneUntilFail,
      s0 = IDLE,
      timeBound = 10.0
    )
    println(s"Probability of not reaching DONE until reaching FAIL from IDLE within 10 time units: $probReachFail")


  def notFail() =
    val alwaysNotFail: Property[State] = stocChannel.never(_ == FAIL)
    val probNotFail = stocChannel.experiment(
      runs = 1000,
      prop = alwaysNotFail,
      s0 = IDLE,
      timeBound = 10.0
    )
    println(s"Probability of never reaching FAIL from IDLE within 10 time units: $probNotFail")

  def stayInSend() =
    val stayInSend: Property[State] = stocChannel.always(_ == SEND)
    val probNotFail = stocChannel.experiment(
      runs = 1000,
      prop = stayInSend,
      s0 = IDLE,
      timeBound = 5.0
    )
    println(s"Probability of always staying in SEND from IDLE within 5 time units: $probNotFail")



  stayInSend()




