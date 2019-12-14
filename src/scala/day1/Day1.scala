package day1

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day1 {

  def main(args: Array[String]): Unit = {
    println(fuelCounterUpper(loadInput()))
  }

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

  val fuelCounterUpper = (modules: List[Int]) => modules.map(element => element / 3 - 2).sum
}
