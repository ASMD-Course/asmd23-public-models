package scala.lab.u06.task2

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

object PNReadersWritersPriorityCheck extends Properties("PNReadersWritersPriorityCheck") :

  import scala.lab.u06.task1.SystemPropertyAnalysis.*
  import scala.lab.u06.task2.PNReadersWritersPriority.readersWritersWithPriority as system
  import PNReadersWritersPriority.*

  given Arbitrary[Marking[Place]] = Arbitrary:
    for
      n <- Gen.choose(1, 5)
    yield
      MSet.ofList(List.fill(n)(P1) ++ List(P5))
  
  val depth = 7

  property("High priority readers go before writers") = forAll: (initial: Marking[Place]) =>
    val highPriorityWaiting: SystemProperty[Marking[Place]] = m => m(P3H) > 0 && m(P5) == 1
    val highPriorityCanGo: SystemProperty[Marking[Place]] = m => system.next(m).exists(mn => mn(P6) > m(P6))

    system.invariant(initial, depth)(highPriorityWaiting implies highPriorityCanGo)

  property("Readers can pass to high priority") = forAll: (initial: Marking[Place]) =>
    val hasNormalReader: SystemProperty[Marking[Place]] = m => m(P3) > 0
    val canAge: SystemProperty[Marking[Place]] = m => system.next(m).exists(mn => mn(P3H) > m(P3H))

    system.invariant(initial, depth)(hasNormalReader implies canAge)

  property("Reader will eventually read") = forAll: (initial: Marking[Place]) =>
    val readerRequest: SystemProperty[Marking[Place]] = m => m(P3) > 0 || m(P3H) > 0
    val willBeServed: SystemProperty[Marking[Place]] = m => system.eventually(m, depth)(m2 => m2(P6) > 0)
    val readerServiceGuarantee: SystemProperty[Marking[Place]] = readerRequest implies willBeServed

    system.invariant(initial, depth)(readerServiceGuarantee)










