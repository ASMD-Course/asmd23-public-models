package scala.lab.u07.task2

import u07.modelling.{CTMC, SPN}
import u07.utils.MSet

import scala.lab.u07.task1.CTMCSimulationExt.*
import java.util.Random
import scala.lab.u07.task1.{meanTimeFor, statesTiming, timingPercentage}

object SRWAnalysis extends App:

  import StocasticReaderWriters.*
  import u07.modelling.SPN.*
  import SPNAnalyzer.*

  val initialMarking = MSet(P1, P5)
  val targetMarking =  MSet(P8, P5)
  val simulationRuns = 100
  val rnd = new Random()

  val prefVariations = Seq(100_000.0, 300_000.0)
  val writeSpeedVariations = Seq(0.1, 0.4)

  val analyzer = SPNAnalyzer[Place](
    Trn(MSet(P1), _ => 1.0, MSet(P2), MSet()),
    Trn(MSet(P2), _ => 200_000, MSet(P3), MSet()).rateValues(prefVariations *),
    Trn(MSet(P2), _ => 100_000, MSet(P4), MSet()).rateValues(prefVariations *),
    Trn(MSet(P3, P5), _ => 100_000, MSet(P6, P5), MSet()),
    Trn(MSet(P4, P5), _ => 100_000, MSet(P7), MSet(P6)),
    Trn(MSet(P6), m => 0.2, MSet(P8), MSet()).rateMultipliers(writeSpeedVariations *),
    Trn(MSet(P7), _ => 0.2, MSet(P8, P5), MSet()).rateMultipliers(writeSpeedVariations *)
  )

  extension (spn: SPN[Place])
    def strTransition: String =
      spn.map: trn =>
        val transition = s"${trn.cond} -> ${trn.eff}" + (if trn.inh.size == 0 then "" else s"  [inh=${trn.inh}]")
        val r = trn.rate(initialMarking)
        f"$transition @ rate $r%.6f"

      .mkString("\n")

  def analyzeParameterVariations(): Unit =
    analyzer.configurations.foreach: spn =>
      println(spn.strTransition)

      val cmc = toCTMC(spn)
      val meanTime = cmc.meanTimeFor(initialMarking, targetMarking, rnd)(simulationRuns)
      val permanence = cmc.statesTiming(initialMarking, targetMarking, rnd)

      println(s"Mean time to reach target: ${meanTime.getOrElse("Not reached")}")
      println(s"Permanence times: ${permanence.getOrElse(Map.empty)}")
      println(s"Timing percentage: ${cmc.timingPercentage(initialMarking, targetMarking, rnd)(simulationRuns)}")
      println("-" * 50)


  analyzeParameterVariations()
