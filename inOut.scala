import java.io._
import scala.io.Source
import annotation.tailrec

object Base {
  def main(args: Array[String]) {
    // Input Reading
    Source.fromFile("input.txt").foreach{
      print
    }

    


    // Output Writing
    val writer = new PrintWriter(new File("output.txt"))

    writer.write("Something")
    writer.close()

  }
}