package day1

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day1 {

  def main(args: Array[String]): Unit = {
    println("Part 1:")
    println(fuelCounterUpper(loadInput()))
    println("Part 2:")
    println(sophisticatedFuelCounterUpper(loadInput()))
  }

  /**
    * Load input as list of integers
    */
  val loadInput = () => {
    val numberList = ListBuffer[Int]()
    val source = Source.fromFile(getClass.getClassLoader.getResource("day1/input1").getPath)
    try {
      for (line <- source.getLines) {
        numberList += line.toInt
      }
    } finally source.close

    numberList.toList
  }

  /**
    * Recursively calculate the fuel required by a single module
    */
  val recursiveFuelSingleModule: Int => Int = (module: Int) => {
    val fuel = module / 3 - 2
    fuel match {
      case x if x < 0 => 0
      case _ => fuel + recursiveFuelSingleModule(fuel)
    }
  }

  /**
    * Count the total fuel required for the simple calculation method
    */
  val fuelCounterUpper = (modules: List[Int]) => modules.map(element => element / 3 - 2).sum

  /**
    * Count the total fuel required for the "sophisticated" method
    */
  val sophisticatedFuelCounterUpper = (modules: List[Int]) => modules.map(
    module => recursiveFuelSingleModule(module)).sum
}
