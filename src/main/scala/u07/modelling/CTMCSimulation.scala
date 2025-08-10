package u07.modelling

import java.util.Random
import u07.utils.Stochastics

object CTMCSimulation:

  case class Event[A](time: Double, state: A)
  type Trace[A] = LazyList[Event[A]]

  export CTMC.*
  export Option.*

  extension [S](self: CTMC[S])

    def newSimulationTrace(s0: S, rnd: Random): Trace[S] =
      LazyList.iterate(Event(0.0, s0)):
        case Event(t, s) =>
          if self.transitions(s).isEmpty
          then
            Event(t, s)
          else
            val choices = self.transitions(s) map (t => (t.rate, t.state))
            val next = Stochastics.cumulative(choices.toList)
            val sumR = next.last._1
            val choice = Stochastics.draw(next)(using rnd)
            Event(t + Math.log(1 / rnd.nextDouble()) / sumR, choice)

    /**
     * The mean time necessary to reach the target state for n simulations
     *
     * @param s0 The starting state
     * @param ts The terminal state (the one that should be evaluated
     * @param rnd A Random number generator instance
     * @param n the number of simulations to run
     *
     * @return An `Option[Double]` containing the mean time to reach the target state, or `None` if it was never reached
     */
    def meanTimeFor(s0: S, ts: S, rnd:Random)(n: Int): Option[Double] =
      val times = for
        _ <- 1 to n
        trace = self.newSimulationTrace(s0, rnd)
        tgt <- trace.find(_.state == ts)
      yield tgt.time

      if times.nonEmpty then Some(times.sum / times.size) else None

    /**
     * Produce a map of permanence for each state reached in the simulation except for the target
     *
     * @param s0 The starting state
     * @param ts The terminal state
     * @param rnd A Random number generator instance
     *
     * @return An `Option` object containing the map of states and their permanence times, or `None`
     *         if the target state was never reached
     */
    def statesTiming(s0: S, ts: S, rnd: Random): Option[Map[S, Double]] =
      val trace = self.newSimulationTrace(s0, rnd)
      trace.zip(trace.tail)
        .takeWhile:
          case (current, _) => current.state != ts
        .map:
          case (current, next) => (current.state, next.time - current.time)
        .toList match
          case Nil if trace.head.state == ts => Some(Map.empty)
          case Nil => None
          case pairs => Some(pairs.groupMapReduce(_._1)(_._2)(_ + _))

    /**
     * Produce a map with the permanence mean for each state reached in the simulation except for the target
     *
     * @param s0 The starting state
     * @param ts The terminal state
     * @param rnd A Random number generator instance
     * @param n   Number of runs
     *
     * @return An `Option` object containing the map of states and their permanence times mean, or `None`
                if the target state was never reached
     */
    def meanStatesTiming(s0: S, ts: S, rnd: Random)(n: Int): Option[Map[S, Double]] =
      val residenceMaps = for
        _ <- 1 to n
        resMap <- statesTiming(s0, ts, rnd)
      yield resMap

      if residenceMaps.isEmpty then
        None
      else
        Some:
          residenceMaps
            .flatMap(_.toSeq)
            .groupMapReduce(_._1)(_._2)(_ + _)
            .view
            .mapValues(_ / residenceMaps.size)          // NOTE FOR THE TASK REPORT APPLY THIS WITHOUT A VIEW CALL IS DEPRECATED
            .toMap

    /**
     * Produce a map with the percentage of permanence for each state reached in the simulation except for the target
     *
     * @param s0 The starting state
     * @param ts The terminal state
     * @param rnd A Random number generator instance
     * @param n   Number of runs
     *
     * @return An `Option` object containing the map of states and their permanence times percentage, or `None`
                if the target state was never reached
     */
    def timingPercentage(s0: S, ts: S, rnd: Random)(n: Int): Option[Map[S, Double]] =
      meanStatesTiming(s0, ts, rnd)(n).map : m =>
        val total = m.values.sum
        m.transform((_, v) => v * 100 / total)

    /**
     * Percentage of permanence for a given state
     *
     * @param s The state for which to compute the percentage of permanence
     * @param s0 The starting state
     * @param ts The terminal state
     * @param rnd A Random number generator instance
     * @param n   Number of runs
     *
     * @return An `Option` object containing the map of states and their permanence times percentage, or `None`
                if the target state was never reached
     */
    def percentageOf(s: S, s0: S, ts: S, rnd: Random)(n: Int): Double =
      timingPercentage(s0, ts, new Random)(n)
        .fold(0.0)(_.getOrElse(s, 0.0))


