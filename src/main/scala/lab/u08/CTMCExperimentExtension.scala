package scala.lab.u08

import u08.modelling.CTMC
import scala.annotation.tailrec


object CTMCExperimentExtension:

  export u08.modelling.CTMCExperiment.*

  /**
   * Creation of a little DSL for chaining together the various Properties
   */
  extension [A](p: Property[A])
    /**
     * Negation of a property
     * @return the negation of the property
     */
    infix def unary_! : Property[A] = trace => !p(trace)

    /**
     * Implication of two properties
     *
     * @param q the other property
     * @return the implication of the two properties
     */
    infix def ==>(q: Property[A]): Property[A] = trace => !p(trace) || q(trace)

    /**
     * Conjunction of two properties
     *
     * @param q the other property
     * @return the conjunction of the two properties
     */
    infix def &&(q: Property[A]): Property[A] = trace => p(trace) && q(trace)

    /**
     * Disjunction of two properties
     *
     * @param q the other property
     * @return the disjunction of the two properties
     */
    infix def ||(q: Property[A]): Property[A] = trace => p(trace) || q(trace)

    /**
     * Until operator
     * @param other the other property
     * @return the Until operation for the two properties
     */
    infix def U(other: Property[A]): Property[A] = trace =>
      import u08.modelling.CTMCSimulation.Trace

      @tailrec
      def checkUntil(remaining: Trace[A]): Boolean =
        remaining.headOption match
          case None => false
          case Some(event) =>
            val singleEventTrace = LazyList(event)
            if other(singleEventTrace) then true
            else if p(singleEventTrace) then checkUntil(remaining.tail)
            else false

      checkUntil(trace)
  

  extension [S](self: CTMC[S])
    /**
     * G operator expressed as G x= not F not x
     *
     * @param filter the filter to apply
     * @tparam A
     * @return
     */
    def always[A](pred: A => Boolean): Property[A] = ! self.eventually[A](p => !pred(p))

    /**
     * Never operator
     *
     * @param filter
     * @tparam A
     * @return
     */
    def never[A](pred: A => Boolean): Property[A] =  self.always(p => !pred(p))

    /**
     * Now operator, which checks the property on the first element of the trace
     *
     * @param pred the predicate to check
     * @tparam A
     * @return
     */
    def now[A](pred: A => Boolean): Property[A] = tr => tr.headOption.exists(ev => pred(ev.state))



