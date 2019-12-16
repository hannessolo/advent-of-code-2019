package day4

object Day4 {

  def lowerBound = 172851

  def upperBound = 675869

  def main(args: Array[String]): Unit = {
    val result = (lowerBound until upperBound)
      // Filter all numbers which have not only decreasing values
      .filter(el => {
      var hasOnlyIncreasing = true
      var previousDigit = el / Math.pow(10, 5).asInstanceOf[Int] % 10
      for (i <- 4 to 0) {
        val currentDigit = (el / Math.pow(10, i).asInstanceOf[Int]) % 10
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
        val currentDigit = (el / Math.pow(10, i).asInstanceOf[Int]) % 10
        if (currentDigit == previousDigit) {
          hasPair = true
        }
        previousDigit = currentDigit
      }
      hasPair
    }).count(_ => true)

    println("There are " + result + " passwords")

  }

}
