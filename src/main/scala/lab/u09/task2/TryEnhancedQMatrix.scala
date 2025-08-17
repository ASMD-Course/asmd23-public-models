package scala.lab.u09.task2

import u09.model.QMatrix.Move.DOWN

object TryEnhancedQMatrix extends App:

  import EnhancedQMatrix.*

  val facade = Facade(
    width = 7,
    height = 7,
    initial = (0, 0),
    terminal = {case _=>false},
    reward = { case ((1,0),DOWN) => 10; case ((3,0),DOWN) => 5; case _ => 0},
    jumps = { case ((1,0),DOWN) => (1,4); case ((3,0),DOWN) => (3,2) },
    items = Set((2, 2), (5, 1), (4, 5)),
    walls = Set(
      Set((1, 1), (1, 2), (1, 3)),
      Set((3, 3), (4, 3), (5, 3)),
      Set((3, 0), (3, 1))
    ),
    gamma = 0.6,
    alpha = 0.5,
    epsilon = 0.5,
    v0 = 1
  )
  facade.printMap(facade.initial)

  val q0 = facade.qFunction
  println(facade.show(q0.vFunction, "%2.2f"))
  val q1 = facade.makeLearningInstance().learn(10000, 100, q0)
  println(facade.show(q1.vFunction, "%2.2f"))
  println(facade.show(s => q1.bestPolicy(s).toString, "%7s"))
