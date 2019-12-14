import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day2 {

  type Program = List[Int]

  def main(args: Array[String]): Unit = {
    val in = readInput()
    // Do the update as required by spec
    val prog = in.updated(1, 12).updated(2, 2)
    println(execute(prog, 0))
  }

  /**
    * Read input from file
    */
  val readInput = () => {
    val numberList = ListBuffer[Int]()
    val source = Source.fromFile(getClass.getClassLoader.getResource("day2/program").getPath)
    try {
      val line = source.getLines.next()
      for (num <- line.split(",")) {
        numberList += num.toInt
      }
      numberList.toList
    } finally source.close
  }

  /**
    * Return the program with the i3th position as i1 * i2 position
    */
  val multiply: (Program, Int, Int, Int) => Program = (prog, i1, i2, i3) => {
    prog.updated(prog(i3), prog(prog(i1)) * prog(prog(i2)))
  }

  /**
    * Return the program with the i3th position as i1 + i2 position
    */
  val add: (Program, Int, Int, Int) => Program = (prog, i1, i2, i3) => {
    prog.updated(prog(i3), prog(prog(i1)) + prog(prog(i2)))
  }

  /**
    * Execute the program
    */
  val execute: (Program, Int) => Int = (prog, pc) => {
    val opcode = prog(pc)

    opcode match {
      case 1 => execute(add(prog, pc + 1, pc + 2, pc + 3), pc + 4)
      case 2 => execute(multiply(prog, pc + 1, pc + 2, pc + 3), pc + 4)
      case 99 => prog.head
      case _ => throw new RuntimeException("Unknown Opcode!" + opcode)
    }
  }
}
