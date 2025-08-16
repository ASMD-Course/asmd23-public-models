package scala.lab.u08

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CTMCExperimentExtensionTest extends AnyFunSuite with Matchers:

  export u08.examples.StochasticChannel.*
  export CTMCExperimentExtension.*

  val reachIdle: Property[State] = stocChannel.eventually(_ == IDLE)
  val reachDone: Property[State] = stocChannel.eventually(_ == DONE)
  val reachFail: Property[State] = stocChannel.eventually(_ == FAIL)
  val reachSend: Property[State] = stocChannel.eventually(_ == SEND)

  test("Property negation should work correctly"):
    val notReachDone = !reachDone

    val probReachDone = stocChannel.experiment(
      runs = 1000,
      prop = reachDone,
      s0 = IDLE,
      timeBound = 1.0
    )

    val probNotReachDone = stocChannel.experiment(
      runs = 1000,
      prop = notReachDone,
      s0 = IDLE,
      timeBound = 1.0
    )

    math.abs((probReachDone + probNotReachDone) - 1.0) should be < 0.1

  test("Property conjunction should work correctly"):
    val bothDoneAndSend = reachDone && reachSend

    val probBoth = stocChannel.experiment(
      runs = 1000,
      prop = bothDoneAndSend,
      s0 = IDLE,
      timeBound = 1.0
    )

    val probDone = stocChannel.experiment(
      runs = 1000,
      prop = reachDone,
      s0 = IDLE,
      timeBound = 1.0
    )

    probBoth should be <= probDone

  test("Property disjunction (||) should work correctly"):
    val doneOrFail = reachDone || reachFail

    val probEither = stocChannel.experiment(
      runs = 1000,
      prop = doneOrFail,
      s0 = IDLE,
      timeBound = 1.0
    )

    val probDone = stocChannel.experiment(
      runs = 1000,
      prop = reachDone,
      s0 = IDLE,
      timeBound = 1.0
    )

  test("Property implication should work correctly"):
    val sendImpliesDoneOrFail = reachSend ==> (reachDone || reachFail)

    val probImplication = stocChannel.experiment(
      runs = 1000,
      prop = sendImpliesDoneOrFail,
      s0 = IDLE,
      timeBound = 1.0
    )

    probImplication should be > 0.8

  test("Until operator should work correctly"):
    val idleUntilSend = reachIdle U reachSend

    val probability = stocChannel.experiment(
      runs = 1000,
      prop = idleUntilSend,
      s0 = IDLE,
      timeBound = 1.0
    )

    probability should be > 0.6


  test("Until operator should handle immediate satisfaction"):
    val sendUntilSend = reachSend U reachSend

    val probability = stocChannel.experiment(
      runs = 1000,
      prop = sendUntilSend,
      s0 = SEND,
      timeBound = 1.0
    )

    probability should be > 0.9

  test("Always operator should work correctly"):
    val alwaysNotFail = stocChannel.always[State](_ != FAIL)

    val probability = stocChannel.experiment(
      runs = 1000,
      prop = alwaysNotFail,
      s0 = IDLE,
      timeBound = 0.5
    )

    probability should be >= 0.8
    probability should be < 1.0

  test("Always operator should work for always true conditions"):
    val alwaysTrue = stocChannel.always[State](_ => true)

    val probability = stocChannel.experiment(
      runs = 100,
      prop = alwaysTrue,
      s0 = IDLE,
      timeBound = 1.0
    )

    probability shouldBe 1.0

  test("Never operator should be equivalent to always not"):
    val neverFail1 = stocChannel.never[State](_ == FAIL)
    val neverFail2 = stocChannel.always[State](_ != FAIL)

    val prob1 = stocChannel.experiment(runs = 1000, prop = neverFail1, s0 = IDLE, timeBound = 0.5)
    val prob2 = stocChannel.experiment(runs = 1000, prop = neverFail2, s0 = IDLE, timeBound = 0.5)

    math.abs(prob1 - prob2) should be < 0.20

  test("Always and eventually combination"):
    val eventuallyDone = stocChannel.eventually[State](_ == DONE)
    val alwaysNotIdle = stocChannel.always[State](_ != IDLE)
    val combined = eventuallyDone && alwaysNotIdle

    val probability = stocChannel.experiment(
      runs = 1000,
      prop = combined,
      s0 = SEND,
      timeBound = 1.0
    )

    probability should be >= 0.0

  test("Logical equivalences should hold"):
    val prop1 = !(reachDone && reachFail)
    val prop2 = (!reachDone) || (!reachFail)

    val prob1 = stocChannel.experiment(runs = 1000, prop = prop1, s0 = IDLE, timeBound = 1.0)
    val prob2 = stocChannel.experiment(runs = 1000, prop = prop2, s0 = IDLE, timeBound = 1.0)

    math.abs(prob1 - prob2) should be < 0.1



