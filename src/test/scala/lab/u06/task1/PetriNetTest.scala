package scala.lab.u06.task1

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import u06.modelling.PetriNet.Marking


object PetriNetTest extends Properties("PetriNet"):

  import scala.lab.u06.task1.SystemPropertyAnalysis.*
  import scala.lab.u06.task1.PNReadersWriters.readersWriters as system
  import scala.lab.u06.task1.PNReadersWriters.Place
  export scala.lab.u06.task1.PNReadersWriters.Place.*
  export u06.utils.MSet
  
  given Arbitrary[Marking[Place]] = 
    Arbitrary:
        for 
          n <- Gen.choose(5, 10)
        yield 
          MSet.ofList(List.fill(n)(P1) ++ List(P5))

  private val depth = 7

  property("Resource token conservation") = forAll: (initial: Marking[Place]) =>
    val resourceCount: SystemProperty[Marking[Place]] = m => m(P5) + m(P7) == 1
    val totalTokenConservation: SystemProperty[Marking[Place]] = m =>
      m.size == initial.size || // Readers active
        m.size == initial.size - 1 && m(P7) == 1  // Writer active

    system.invariant(initial, depth)(resourceCount and totalTokenConservation)

  property("System makes progress under contention") = forAll: (initial: Marking[Place]) =>
    val multipleWaiting: SystemProperty[Marking[Place]] = m => (m(P3) + m(P4)) >= 2 && m(P5) == 1
    val someoneProceeds: SystemProperty[Marking[Place]] = m => system.next(m)
      .exists(next => next(P6) > m(P6) || next(P7) > m(P7))

    multipleWaiting(initial) implies system.invariant(initial, depth)(someoneProceeds)

  property("Mutual exclusion for path") = forAll: (initial: Marking[Place]) =>
    val noReadersWritersTogether: SystemProperty[Marking[Place]] = m => !(m(P6) > 0 && m(P7) > 0)
    val atMostOneWriter: SystemProperty[Marking[Place]] = m => m(P7) <= 1

    system.invariant(initial, depth)(noReadersWritersTogether and atMostOneWriter)

  property("No deadlock encountered") = forAll: (initial: Marking[Place]) =>
    val isDeadlocked: SystemProperty[Marking[Place]] = m => system.next(m).isEmpty
    val isValidTermination: SystemProperty[Marking[Place]] = m => m == MSet(P5) || m == MSet.ofList(Nil)

    system.invariant(initial, depth)(!isDeadlocked or isValidTermination)

  property("Processes eventually become active") = forAll: (initial: Marking[Place]) =>
    val aProcessIsPresent: SystemProperty[Marking[Place]] = m => m(P1) > 0 || m(P5) == 1
    val isReaderOrWriter: SystemProperty[Marking[Place]] = m => m(P6) > 0 || m(P7) > 0

    aProcessIsPresent(initial) implies system.eventually(initial, depth)(isReaderOrWriter)

  property("Active processes eventually complete") = forAll: (initial: Marking[Place]) =>
    val wasActive: SystemProperty[Marking[Place]] = m => m(P6) > 0 || m(P7) > 0
    val someoneReturned: SystemProperty[Marking[Place]] = m => m(P1) == initial(P1)

    system.eventually(initial, depth)(wasActive) implies system.eventually(initial, depth)(someoneReturned)

  property("Writer fairness") = forAll : (initial: Marking[Place]) =>
    val isWriterReady: SystemProperty[Marking[Place]] = m => m(P4) > 0 && m(P5) == 1
    val noReaders: SystemProperty[Marking[Place]] = m => m(P6) == 0
    val someoneIsWriting: SystemProperty[Marking[Place]] = m => m(P7) > 0 && m(P5) == 0

    system.eventually(initial, depth)((isWriterReady and noReaders) implies someoneIsWriting)

  property("Reader fairness") = forAll : (initial: Marking[Place]) =>
    val isReaderReady: SystemProperty[Marking[Place]] = m => m(P3) > 0 && m(P5) == 1
    val noWriters: SystemProperty[Marking[Place]] = m => m(P7) == 0
    val someoneIsReading: SystemProperty[Marking[Place]] = m => m(P6) > 0 && m(P5) == 1

    system.eventually(initial, depth)((isReaderReady and noWriters) implies someoneIsReading)


  
  
  
      