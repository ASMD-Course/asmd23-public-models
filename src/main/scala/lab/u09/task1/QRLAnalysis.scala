package scala.lab.u09.task1

import u09.model.QRL

import scala.annotation.tailrec

trait QRLAnalysis extends QRL:

  type Configuration
  type Result = (Configuration, Q)

  trait Generator[A] extends (() => LazyList[A]):

    def map[B](f: A => B): Generator[B] = () => this().map(f)

    def flatMap[B](f: A => Generator[B]): Generator[B] = () => this().flatMap(a => f(a)())

  object Generator:

    def fromRange(start: Double, end: Double, step: Double): Generator[Double] =
      () => LazyList
        .iterate(start)(_ + step)
        .takeWhile(_ <= end)

    def fixed(elements: List[Double]): Generator[Double] = () => LazyList.from(elements)

  type ConfigurationGenerator = Generator[Configuration]

  extension (configuration: Configuration)
    def toLearningProcess: LearningProcess


  extension (lp: LearningProcess)
    def learnWithDisplay(episodes: Int, episodeLength: Int, qf: Q, displayInterval: Int, show: Q => String): Q =

      @tailrec
      def learnRec(e: Int, currentQ: Q): Q =
        if e != episodes && e % displayInterval == 0 then
          println(s"Episode ${episodes - e}:")
          println(show(currentQ))

        e match
          case 0 => currentQ
          case _ =>
            val newQ = lp.runSingleEpisode((lp.system.initial, currentQ), episodeLength)._2
            learnRec(e - 1, newQ)

      println("Episode 0:")
      println(show(qf))
      learnRec(episodes, qf)

  def analysis(
      generator: ConfigurationGenerator,
      show: Result => String
  )(episodes: Int, length: Int, qf: Q, interval: Int = 2500): Unit =
    generator().foreach :
      config =>
        println(f"\n${"="*60}")
        println(s"Configuration: $config")
        println(f"${"="*60}")


        val lp = config.toLearningProcess
        lp.learnWithDisplay(
          episodes,
          length,
          qf.copy(),
          interval,
          q => show(config, q)
        )








