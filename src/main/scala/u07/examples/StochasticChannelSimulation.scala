package u07.examples


import java.util.Random
import u07.examples.StochasticChannel.*

import scala.lab.u07.task1.*

@main def mainStochasticChannelSimulation =

  println:
    //stocChannel.timingPercentage(IDLE, DONE, new Random)(30)
    stocChannel.statesTiming(IDLE, DONE, new Random)



//
//  Time.timed:
//    println:
//      stocChannel.newSimulationTrace(IDLE, new Random)
//        .take(10)
//        .toList
//        .mkString("\n")

