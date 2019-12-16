package day4

object Day4 {

  def lowerBound = 172851

  def upperBound = 675869

  // Nth digit, little-endian
  private val nthDigit = (number: Int, digit: Int) => (number / Math.pow(10, digit).asInstanceOf[Int]) % 10

  def main(args: Array[String]): Unit = {
    val part1 = (lowerBound until upperBound)
      // Filter all numbers which have not only decreasing values
      .filter(el => {
      var hasOnlyIncreasing = true
      var previousDigit = nthDigit(el, 5)
      for (i <- 4 to 0 by -1) {
        val currentDigit = nthDigit(el, i)
        if (currentDigit < previousDigit) {
          hasOnlyIncreasing = false
        }
        previousDigit = currentDigit
      }
      hasOnlyIncreasing
    })
      // Filter all numbers which don't have a 2 identical adjacent digits
      .filter(el => {
      var previousDigit = el % 10
      var hasPair = false
      for (i <- 1 to 5) {
        val currentDigit = nthDigit(el, i)
        if (currentDigit == previousDigit) {
          hasPair = true
        }
        previousDigit = currentDigit
      }
      hasPair
    })

    val resultPart1 = part1.count(_ => true)

    println("There are " + resultPart1 + " passwords")

    // Remove results where group of two is part of larger group
    val resultPart2 = part1.filter(el => {
      val compareDigit = (number: Int, pos: Int) => {
        pos match {
          case 0 => nthDigit(number, pos) == nthDigit(number, pos + 1) && nthDigit(number, pos + 1) != nthDigit(number, pos + 2)
          case 5 => nthDigit(number, pos) == nthDigit(number, pos + 1) && nthDigit(number, pos) != nthDigit(number, pos - 1)
          case _ => nthDigit(number, pos) == nthDigit(number, pos + 1) && nthDigit(number, pos) != nthDigit(number, pos - 1) && nthDigit(number, pos + 1) != nthDigit(number, pos + 2)
        }
      }
      var groupOf2NotPartOfLargerGroup = false
      for (i <- 0 to 5) {
        groupOf2NotPartOfLargerGroup ||= compareDigit(el, i)
      }
      groupOf2NotPartOfLargerGroup
    }).count(_ => true)

    println("There are " + resultPart2 + " passwords")

  }

}
