package scala.lab.u07.task3


import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.util.Random
import u07.utils.Stochastics.*

class StochasticsTest extends AnyFunSuite with Matchers:

  given Random = new Random(42)

  test("statistics should return correct total count"):
    val choices = Set((0.3, "A"), (0.5, "B"), (0.2, "C"))
    val size = 1000
    val result = statistics(choices, size)
    result.values.sum shouldBe size


  test("statistics should only return values from original choices"):

    val choices = Set((0.4, "X"), (0.6, "Y"))
    val size = 500

    val result = statistics(choices, size)
    val expectedValues = choices.map(_._2)

    result.keys.toSet shouldBe expectedValues
  

  test("statistics should approximate expected probabilities"):
    val choices = Set(
      (0.1, "rare"),
      (0.3, "common"),
      (0.6, "frequent")
    )
    val size = 10000
    val result = statistics(choices, size)
    val tolerance = 0.05

    val rareCount = result.getOrElse("rare", 0)
    val commonCount = result.getOrElse("common", 0)
    val frequentCount = result.getOrElse("frequent", 0)

    (rareCount.toDouble / size) should be (0.1 +- tolerance)
    (commonCount.toDouble / size) should be (0.3 +- tolerance)
    (frequentCount.toDouble / size) should be (0.6 +- tolerance)


  test("statistics should handle single choice"):
    val choices = Set((1.0, "only"))
    val size = 50
    val result = statistics(choices, size)
    result shouldBe Map("only" -> 50)


  test("statistics should handle zero size"):
    val choices = Set(
      (0.5, "A"),
      (0.5, "B")
    )
    val size = 0
    val result = statistics(choices, size)
    result shouldBe Map.empty[String, Int]


  test("statistics should work with different data types"):
    case class Item(name: String, value: Int)
    val choices = Set(
      (0.4, Item("sword", 100)),
      (0.6, Item("shield", 80))
    )
    val size = 200

    val result = statistics(choices, size)
    result.values.sum shouldBe size
    result.keys.foreach:
      item =>
        choices.map(_._2) should contain(item)

