import java.io._
import scala.io.Source
import annotation.tailrec

object Base {
  def main(args: Array[String]) {
    // Notice the lack of semicolons
    // Input Reading
    val input_name = "square_detector.txt"
    val input = Source.fromFile(input_name).getLines.toArray

    val num_of_cases: Int = input.head.toInt
    var bools = new Array[Boolean](num_of_cases)
    var count = 0

    // If you don't put this it doesn't do tail recursion
    @ tailrec
    def squareSplitter(cases: Array[String]): Unit =
      // Splits giant file into individual cases
      if (cases.isEmpty) println("Finished")
      else {
        val N: Int = cases.head.toInt
        val matrix = cases.slice(1,N+1)
        bools(count) = isSquare(N, matrix)
        count += 1
        squareSplitter( cases.drop( N + 1 ) )
      }

    def isSquare(N: Int, matrix: Array[String]): Boolean = {
      // Solves Individual cases
      def endsStripper(strArr: Array[String]): Array[String] = {

        // Removes empty strings from ends of array, tail recursively
        @ tailrec
        def endStripper(strArr1: Array[String]): Array[String] = {
            if (strArr1.head.isEmpty) endStripper(strArr1.tail)
            else strArr1
        }
        // Why isn't there a nice functional way 
        // to get the last element of a list?
        endStripper(endStripper(strArr).reverse)
      }
      
      def lineEndsStripper(str: String): String = {
        // Strips Away periods/white cells on the outer edges
        @ tailrec
        def lineEndStripper(str1:String): String =
          if (str1.isEmpty || str1.head != '.') str1
          else lineEndStripper(str1.tail)

        // Seriously, why not?
        lineEndStripper(lineEndStripper(str).reverse)
      }

      // yield is like return so every iteration it returns a String 
      // so it becomes a List of Strings
      // like for(i <- (1 to 3)) yield i == Array(1, 2, 3)
      val newMat = endsStripper (for ( line <- matrix ) yield lineEndsStripper(line))

      val len = newMat.head.length

      // For all returns true only if the condition is true for all elements
      newMat.forall(each => !each.contains('.') && each.length == len ) && (len == newMat.length)
    }
    squareSplitter(input.tail)
    
    // Output Writing
    count = 1
    // Stupid Hack to make true => YES and false => NO
    val yesNos = for(bool <- bools) yield (if (bool) "YES" else "NO")
    val writer = new PrintWriter(new File("output.txt"))
    for(i <- 1 to num_of_cases) writer.write("Case #" + i + ": " + yesNos(i-1) +"\n")
    writer.close()
  }
}