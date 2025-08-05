package u06.examples

export u06.modelling.PetriNet
import u06.utils.MSet

object PNMutualExclusion:

  enum Place:
    case N, T, C
    
  export Place.*
  export u06.modelling.PetriNet.*
  export u06.modelling.SystemAnalysis.*
  export u06.utils.MSet

  // DSL-like specification of a Petri Net
  def pnME = PetriNet[Place](
    MSet(N) ~~> MSet(T),                    // Pass from Normal to Trying
    MSet(T) ~~> MSet(C) ^^^ MSet(C),        // Pass from Trying to Critical if and only if no one is on critical
    MSet(C) ~~> MSet()                      // From Critical I go to empty status
  )

  def sysPN = pnME.toSystem

@main def mainPNMutualExclusion =
  import PNMutualExclusion.*

  //println(pnME)

  // example usage
  println(sysPN.paths(MSet(N,N),7).toList.mkString("\n"))

  // Output:
  //  List({N|N}, {T|N}, {T|T}, {C|T}, {T}, {C}, {})
  //  List({N|N}, {T|N}, {C|N}, {C|T}, {T}, {C}, {})
  //  List({N|N}, {T|N}, {C|N}, {N}, {T}, {C}, {})
  
  // Remember that in MSet {N_1 | N_2} -> {T | N_2} == {N_1 | N_2} -> {N_1 | T} 
  // Actually, the transitions are considered the same 
