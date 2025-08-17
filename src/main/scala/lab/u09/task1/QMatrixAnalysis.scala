package scala.lab.u09.task1

import u09.model.QMatrix.Move
import u09.model.QMatrix.Move.*
import u09.model.QRLImpl

object QMatrixAnalysis extends QRLAnalysis with QRLImpl with App:

  export u09.model.QMatrix.*

  type Configuration = Facade


  extension (c: Configuration)
    def toLearningProcess: LearningProcess = c.makeLearningInstance().asInstanceOf[LearningProcess]

  val configGen: ConfigurationGenerator =
    for
      g <- Generator.fromRange(0.1, 0.9, 0.2)
      a <- Generator.fromRange(0.1, 0.9, 0.2)
      e <- Generator.fixed(List(0.0, 0.5, 0.78, 0.99))
    yield {
      Facade(
        width = 5,
        height = 5,
        initial = (0, 0),
        terminal = { case _ => false },
        reward = { case ((1,0),DOWN) => 10; case ((3,0),DOWN) => 5; case _ => 0},
        jumps = { case ((1,0),DOWN) => (1,4); case ((3,0),DOWN) => (3,2) },
        gamma = g,
        alpha = a,
        epsilon = e,
        v0 = 1
      )
    }

  override type Action = Move
  override type State = Node

  def showGrid(config: Configuration, q: Q): String =
    config match
      case f: Facade =>
        f.show(q.vFunction, "%5.2f")
        f.show(s => q.bestPolicy(s).toString, "%7s")

  val base = QFunction(Move.values.toSet, 1.0, _ => false)

  analysis(configGen, showGrid)(10_000, 100, base)


