package u07.examples

import u07.utils.Time
import java.util.Random
import u07.examples.StochasticChannel.*

@main def mainStochasticChannelSimulation =

  print:
    stocChannel.timingPercentage(IDLE, DONE, new Random)(30)



//  println:
//    stocChannel.meanHittingTime(IDLE, DONE, new Random)(1000)
//
//  Time.timed:
//    println:
//      stocChannel.newSimulationTrace(IDLE, new Random)
//        .take(10)
//        .toList
//        .mkString("\n")

