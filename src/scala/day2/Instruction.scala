package day2

object Instruction extends Enumeration {
  type Instruction = Value
  val add, mul, noop, invalid = Value
}
