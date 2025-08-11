package u07.examples


import java.util.Random
import u07.examples.StochasticChannel.*

import scala.lab.u07.task1.timingPercentage

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

