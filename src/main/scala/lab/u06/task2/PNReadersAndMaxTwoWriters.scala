package scala.lab.u06.task2

object PNReadersAndMaxTwoWriters:

  enum Place:
    case P1,P2,P3,P4,P5,P6,P7,P8

  export Place.*
  export u06.utils.MSet
  export u06.modelling.PetriNet.*
  export u06.modelling.PetriNet

  export u06.modelling.SystemAnalysis.*

  def readersAndMaxTwoWriters = PetriNet[Place](

    MSet(P1) ~~> MSet(P2),

    MSet(P2) ~~> MSet(P3),

    MSet(P8, P2) ~~> MSet(P4),

    MSet(P3, P5) ~~> MSet(P6, P5),

    MSet(P4, P5) ~~> MSet(P7) ^^^ MSet(P6),

    MSet(P6) ~~> MSet(P1),

    MSet(P7) ~~> MSet(P5, P1, P8)
  ).toSystem
  
  

