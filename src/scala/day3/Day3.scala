package day3

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day3 {

  trait LargeInitialHashMap[A, B] extends mutable.HashMap[A, B] {
    override def initialSize: Int = 20000
  }

  def main(args: Array[String]): Unit = {
    val (firstPath, secondPath) = loadInput()
    val pointTable = new mutable.HashMap[(Int, Int), Boolean]() with LargeInitialHashMap[(Int, Int), Boolean]
    val candidates = new mutable.HashSet[(Int, Int)]()

    val move: ((Int, Int), Char, Int, (Int, Int) => Unit) => (Int, Int) = (pos, dir, n, action) => {
      var newPos = pos
      val (dx, dy) = dir match {
        case 'U' => (0, -1)
        case 'D' => (0,  1)
        case 'L' => (-1, 0)
        case 'R' => ( 1, 0)
        case _ => throw new RuntimeException("Unknown direction: " + dir)
      }
      for (_ <- 0 until n) {
        val (x, y) = newPos
        newPos = (x + dx, y + dy)
        action.tupled(newPos)
      }
      newPos
    }

    // Insert phase
    var location = (0, 0)
    for (inst <- firstPath) {
      val direction = inst.charAt(0)
      val number = inst.substring(1).toInt
      location = move(location, direction, number, (x, y) => {
        pointTable.put((x, y), true)
      })
      println("Vertex: " + location)
    }

    // Lookup phase
    location = (0, 0)
    for (inst <- secondPath) {
      val direction = inst.charAt(0)
      val number = inst.substring(1).toInt
      location = move(location, direction, number, (x, y) => {
        val pointExists = pointTable.get(x, y)
        if (pointExists.isDefined && pointExists.get) {
          candidates += ((x, y))
        }
      })
    }

    // Find shortest path
    var best = (0, 0, Int.MaxValue)
    for ((x, y) <- candidates) {
      val (_, _, bestVal) = best
      val value = Math.abs(x) + Math.abs(y)
      println("Candidate: x: " + x + " y: " + y + " value " + value)
      if (value < bestVal) {
        best = (x, y, value)
      }
    }
    val (bestX, bestY, dist) = best
    println("x: " + bestX + " y: " + bestY + " dist: " + dist)

  }

  /**
    * Load input as list of integers
    */
  private val loadInput = () => {
    val firstPath = ListBuffer[String]()
    val secondPath = ListBuffer[String]()
    val source = Source.fromFile(getClass.getClassLoader.getResource("day3/input").getPath)
    try {
      val lines = source.getLines()
      var line = lines.next()
      line.split(",").foreach(instr => firstPath += instr)
      line = lines.next()
      line.split(",").foreach(instr => secondPath += instr)
    } finally source.close

    (firstPath.toList, secondPath.toList)
  }

}
