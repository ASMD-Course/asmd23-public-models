package scala.lab.u06.task3

import u06.utils.MSet

object PriorityPetriNet:

  import u06.modelling.System

  case class Trn[P](cond: MSet[P], eff: MSet[P], inh: MSet[P], priority: Marking[P] => Int = (p:Marking[P]) => 0)

  type PetriNet[P] = Set[Trn[P]]
  type Marking[P] = MSet[P]

  def apply[P](transitions: Trn[P]*): PetriNet[P] = transitions.toSet

  extension [P](pn: PetriNet[P])
    def toSystem: System[Marking[P]] = m =>
      val enabledWithOutput = for
        Trn(cond, eff, inh, priorityFn) <- pn
        if m disjoined inh
        out <- m extract cond
      yield (out, eff, priorityFn(m))

      enabledWithOutput match
        case empty if empty.isEmpty => Set.empty
        case enabled =>
          val minPriority = enabled.map((_, _, p) => p).min
          enabled.collect :
            case (out, eff, p) if p == minPriority => out union eff
  
  extension [P](self: Marking[P])
    infix def ~~>(y: Marking[P]): Trn[P] = Trn(self, y, MSet(), _ => 0)

  extension [P](self: Trn[P])
    infix def ^^^(z: Marking[P]): Trn[P] = self.copy(inh = z)
    infix def withPriority(fn: Marking[P] => Int): Trn[P] = self.copy(priority = fn)
    infix def withPriority(p: Int): Trn[P] = self.copy(priority = _ => p)



