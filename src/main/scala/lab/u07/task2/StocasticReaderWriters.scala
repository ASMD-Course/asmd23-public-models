package scala.lab.u07.task2

import u07.modelling.{CTMC, SPN}
import u07.utils.MSet

import scala.lab.u07.task1.{meanTimeFor, statesTiming, timingPercentage}

object StocasticReaderWriters:

  enum Place:
    case P1, P2, P3, P4, P5, P6, P7

  export Place.*
  export u07.modelling.CTMCSimulation.*
  import u07.modelling.SPN.{Trn, toCTMC}

  val transitions = SPN[Place](
    Trn(MSet(P1), m => 1.0,   MSet(P2),  MSet()),                         // t1
    Trn(MSet(P2), m => 200_000,  MSet(P3),  MSet()),                      // t2
    Trn(MSet(P2), m => 100_000,  MSet(P4),  MSet()),                      // t3
    Trn(MSet(P3, P5), m => 100_000,  MSet(P6, P5),  MSet()),              // t4
    Trn(MSet(P4, P5), m => 100_000,  MSet(P7),  MSet(P6)),                // t5
    Trn(MSet(P6), m => 0.1 * m(P6),  MSet(P1),  MSet() ),                 // t6
    Trn(MSet(P7), m => 0.2,  MSet(P1, P5),  MSet()),                      // t7
  )

  def cmc = toCTMC(transitions)

@main
def test =
  import StocasticReaderWriters.*
  import scala.lab.u07.task1.CTMCSimulationExt.*

//  cmc.newSimulationTrace(MSet(P1, P1, P5), new java.util.Random)
//    .take(20)
//    .toList
//    .foreach(println)


  import u07.modelling.SPN.{Trn, toCTMC}
  import SPNAnalyzer.*

  val variationSequence = Seq(50_000.0, 100_000.0, 150_000.0, 200_000.0)

  val analyzer = SPNAnalyzer[Place](
    Trn(MSet(P1), _ => 1.0, MSet(P2), MSet()),
    Trn(MSet(P2), _ => 100_000, MSet(P3), MSet()).rateValues(variationSequence*),
    Trn(MSet(P2), _ => 100_000, MSet(P4), MSet()).rateValues(variationSequence*),
    Trn(MSet(P3, P5), _ => 100_000, MSet(P6, P5), MSet()),
    Trn(MSet(P4, P5), _ => 100_000, MSet(P7), MSet(P6)),
    Trn(MSet(P6), m => 0.1 * m(P6), MSet(P1), MSet()),
    Trn(MSet(P7), _ => 0.2, MSet(P1, P5), MSet())
  )


  analyzer.configurations.foreach: spn =>
    val cmc = toCTMC(spn)
    println(cmc.timingPercentage(MSet(P1, P1, P5), MSet(P1, P6, P5), new java.util.Random)(100))


//  println:
//    cmc.timingPercentage(MSet(P1, P1, P5), MSet(P1, P6, P5), new java.util.Random)(1000)
