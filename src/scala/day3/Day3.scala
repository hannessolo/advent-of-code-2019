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
    val pointTable = new mutable.HashMap[(Int, Int), Int]() with LargeInitialHashMap[(Int, Int), Int]
    // Store position, total wire length
    val candidates = new mutable.HashSet[(Int, Int, Int)]()

    /**
      * Execute a move. Takes the current position, a direction, and number of steps.
      * Calls callback at each step with the new position, and the number of moves so far.
      * Returns the new position
      */
    val move: ((Int, Int), Char, Int, (Int, Int, Int) => Unit) => (Int, Int) = (pos, dir, n, action) => {
      var newPos = pos
      val (dx, dy) = dir match {
        case 'U' => (0, -1)
        case 'D' => (0,  1)
        case 'L' => (-1, 0)
        case 'R' => ( 1, 0)
        case _ => throw new RuntimeException("Unknown direction: " + dir)
      }
      for (i <- 0 until n) {
        val (x, y) = newPos
        newPos = (x + dx, y + dy)
        action(newPos._1, newPos._2, i + 1)
      }
      newPos
    }

    // Insert phase
    var location = (0, 0)
    var cableLength = 0
    for (inst <- firstPath) {
      val direction = inst.charAt(0)
      val number = inst.substring(1).toInt
      location = move(location, direction, number, (x, y, n) => {
        // We only care about the first time we touch a point
        if (pointTable.get(x, y).isEmpty) {
          pointTable.put((x, y), cableLength + n)
        }
      })
      // Increment the total length we have traversed so far
      cableLength += number
    }

    // Lookup phase
    location = (0, 0)
    cableLength = 0
    for (inst <- secondPath) {
      val direction = inst.charAt(0)
      val number = inst.substring(1).toInt
      location = move(location, direction, number, (x, y, n) => {
        val pointAtCableLength = pointTable.get(x, y)
        if (pointAtCableLength.isDefined) {
          candidates += ((x, y, pointAtCableLength.get + cableLength + n))
        }
      })
      cableLength += number
    }

    // Find shortest path (manhattan distance)
    var best = (0, 0, Int.MaxValue)
    for ((x, y, _) <- candidates) {
      val (_, _, bestVal) = best
      val value = Math.abs(x) + Math.abs(y)
      if (value < bestVal) {
        best = (x, y, value)
      }
    }
    val (bestX, bestY, dist) = best
    println("Shortest by manhattan distance: x: " + bestX + " y: " + bestY + " dist: " + dist)

    // Find shortest path (cable length)
    best = (0, 0, Int.MaxValue)
    for ((x, y, cl) <- candidates) {
      val (_, _, bestVal) = best
      if (cl < bestVal) {
        best = (x, y, cl)
      }
    }
    println("Shortest by cable length: x: " + best._1 + " y: " + best._2 + " dist: " + best._3)

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
