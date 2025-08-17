package scala.lab.u09.task2

import u09.model.QRLImpl

import scala.collection.mutable

object EnhancedQMatrix:

  import u09.model.QMatrix.Move.*
  import u09.model.QMatrix.Move
  import u09.model.QMatrix.Node

  type Wall = Set[Node]

  case class Facade(
   width: Int,
   height: Int,
   initial: Node,
   terminal: PartialFunction[Node, Boolean],
   reward: PartialFunction[(Node, Move), Double],
   jumps: PartialFunction[(Node, Move), Node],
   walls: Set[Wall],
   wallsPenality: Double = -1.5,
   items: Set[Node] = Set(),
   itemsReward: Double = 100,
   returnWithoutItemPenality: Double = -50,
   gamma: Double,
   alpha: Double,
   epsilon: Double = 0.0,
   v0: Double
 ) extends QRLImpl:

    type State = Node
    type Action = Move

    val collectedItems: mutable.Set[Node] = mutable.Set()
    def qEnvironment(): Environment = (s: Node, a: Move) =>
      val intended: Node = (s, a) match
        case ((n1, n2), UP) => (n1, n2 - 1)
        case ((n1, n2), DOWN) => (n1, n2 + 1)
        case ((n1, n2), LEFT) => (n1 - 1, n2)
        case ((n1, n2), RIGHT) => (n1 + 1, n2)

      val hitsWall = walls.exists(_.contains(intended))
      val outOfBounds =
        intended._1 < 0 || intended._1 >= width ||
          intended._2 < 0 || intended._2 >= height

      val n2: Node = if (!hitsWall && !outOfBounds) intended else s

      val baseReward = reward.apply((s, a))
      val afterWallReward = if (hitsWall) baseReward + wallsPenality else baseReward

      val collectionReward =
        if items.contains(n2) && !collectedItems.contains(n2) then
          collectedItems += n2
          itemsReward
        else if n2 == initial && collectedItems == items then
          itemsReward * 5
        else if n2 == initial && collectedItems.nonEmpty then
          returnWithoutItemPenality
        else
          0.0

      val finalReward = afterWallReward + collectionReward
      (finalReward, jumps.orElse[(Node, Move), Node](_ => n2)(s, a))

    def qFunction = QFunction(Move.values.toSet, v0, terminal)
    def qSystem = QSystem(environment = qEnvironment(), initial, terminal)
    def makeLearningInstance() = QLearning(qSystem, gamma, alpha, epsilon, qFunction)

    def show[E](v: Node => E, formatString: String): String =
      (for
        row <- 0 until width
        col <- 0 until height
      yield formatString.format(v((col, row))) + (if (col == height - 1) "\n" else "\t"))
        .mkString("")

    def printMap(agent: Node): Unit =
      for y <- 0 until height do
        val row = (0 until width).map { x =>
          val node = (x, y)

          if agent == node then "A"
          else if walls.exists(_.contains(node)) then "#"
          else if items.contains(node) && !collectedItems.contains(node) then "*"
          else "_"
        }
        println(row.mkString(" "))
      println()




