package scala.lab.u06.task1

import u06.modelling.PetriNet

object PNReadersWriters:
  export u06.utils.MSet

  enum Place:
    case P1,P2,P3,P4,P5,P6,P7

  export Place.*
  export u06.modelling.PetriNet.*
  export u06.modelling.SystemAnalysis.*

  def readersWriters = PetriNet[Place](
    // t1
    MSet(P1) ~~> MSet(P2),

    // t2, t3
    MSet(P2) ~~> MSet(P3), MSet(P2) ~~> MSet(P4),

    // t4
    MSet(P3, P5) ~~> MSet(P6, P5),

    // t5
    MSet(P4, P5) ~~> MSet(P7) ^^^ MSet(P6),

    // t6
    MSet(P6) ~~> MSet(P1),

    // t7
    MSet(P7) ~~> MSet(P5, P1)
  ).toSystem

@main
def tryThis() =
  import PNReadersWriters.*

  println(readersWriters.paths(MSet(P1,P1,P5), 5).mkString("\n"))

