package scala.lab.u06.task1

import u06.modelling.System
import u06.modelling.SystemAnalysis.*

object SystemPropertyAnalysis :

  type SystemProperty[S] = S => Boolean

  extension [S](system: System[S])
    def invariant(s: S, depth: Int)(property: SystemProperty[S]): Boolean =
      system.paths(s, depth).forall(_.forall(property))

    def eventually(s: S, depth: Int)(property: SystemProperty[S]): Boolean =
      system.paths(s, depth).exists(_.exists(property))

    def never(s: S, depth: Int)(property: SystemProperty[S]): Boolean = !eventually(s, depth)(property)

  extension [S](p: SystemProperty[S])
    infix def and(q: => SystemProperty[S]): SystemProperty[S] = s => p(s) && q(s)
    infix def or(q: => SystemProperty[S]): SystemProperty[S] = s => p(s) || q(s)

    infix def not: SystemProperty[S] = s => !p(s)
    infix def unary_! : SystemProperty[S] = not

    infix def implies(q: => SystemProperty[S]): SystemProperty[S] = s => !p(s) || q(s)
  
  extension [S](b: Boolean)
    infix def implies(q: =>Boolean): Boolean = !b || q

