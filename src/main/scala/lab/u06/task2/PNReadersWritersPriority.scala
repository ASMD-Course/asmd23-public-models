package scala.lab.u06.task2

import u06.modelling

object PNReadersWritersPriority:

  enum Place:
    case P1,P2,P3,P3H,P4,P5,P6,P7

  export Place.*
  export u06.utils.MSet
  export u06.modelling.PetriNet.*
  export u06.modelling.PetriNet

  export u06.modelling.SystemAnalysis.*


  def readersWritersWithPriority = PetriNet[Place](
    // t1
    MSet(P1) ~~> MSet(P2),

    // t2
    MSet(P2) ~~> MSet(P3),

    // t3
    MSet(P2) ~~> MSet(P4),

    //t4
    MSet(P3) ~~> MSet(P3H),

    //t6
    MSet(P3H, P5) ~~> MSet(P6, P5),

    // t7
    MSet(P3, P5) ~~> MSet(P6, P5) ^^^ MSet(P3H),  // Prevent firing if token in priority place

    // t8
    MSet(P4, P5) ~~> MSet(P7) ^^^ MSet(P6, P3H),  // Prevent firing if I've a reader or priority is not fulfilled

    // t8
    MSet(P6) ~~> MSet(P1),

    // t9
    MSet(P7) ~~> MSet(P5, P1)
  ).toSystem


