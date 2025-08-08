package scala.lab.u06.task3

object PriorityRW :

  export u06.utils.MSet
  export lab.u06.task1.SystemPropertyAnalysis.*
  import PriorityPetriNet.*

  enum Place:
    case P1, P2, P3, P4, P5, P6, P7

  import Place.*

  val adaptivePetriNet = PriorityPetriNet[Place](
    MSet(P1) ~~> MSet(P2),

    (MSet(P2) ~~> MSet(P3)).withPriority(m=> if m(P3) > 3 then 10 else 1),

    (MSet(P2) ~~> MSet(P4)).withPriority(m =>  if m(P4) > 2 then 0 else 5),

    (MSet(P3, P5) ~~> MSet(P6, P5)).withPriority(m => if m(P4) > 0 then 10 else 0)
    ,

    ((MSet(P4, P5) ~~> MSet(P7)) ^^^ MSet(P6)).withPriority: m =>
      val totalWaiting = m(P3) + m(P4)
      if totalWaiting > 5 then 0
      else if m(P3) > 0 then 20
      else 5
    ,
    (MSet(P6) ~~> MSet(P1)).withPriority(10),
    (MSet(P7) ~~> MSet(P5, P1)).withPriority(10)
  ).toSystem

@main
def testPriorityRW() =
  import PriorityRW.*
  import PriorityRW.Place.*
  import u06.modelling.SystemAnalysis.paths

  println(adaptivePetriNet.paths(MSet(P1,P1,P5), 10).mkString("\n"))