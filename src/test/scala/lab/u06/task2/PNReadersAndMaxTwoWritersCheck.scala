package scala.lab.u06.task2

import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}

import scala.lab.u06.task1.SystemPropertyAnalysis.{SystemProperty, invariant}
import scala.lab.u06.task2.PNReadersAndMaxTwoWriters.Place

object PNReadersAndMaxTwoWritersCheck extends Properties("PNReadersAndMaxTwoWritersCheck"):

  export PNReadersAndMaxTwoWriters.*
  import PNReadersAndMaxTwoWriters.readersAndMaxTwoWriters as system

  given Arbitrary[Marking[Place]] = Arbitrary:
    for
      n <- Gen.choose(5, 10)
    yield
      MSet.ofList(List.fill(n)(P1) ++ List(P5) ++ List(P8, P8))

  private val depth = 7

  property("No more than two writers are present") = forAll: (initial: Marking[Place]) =>
    val atMostTwoWriters: SystemProperty[Marking[Place]] = m => m(P4) <= 2
    system.invariant(initial, depth)(atMostTwoWriters)










