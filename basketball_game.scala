import java.io._
import scala.io.Source
import annotation.tailrec

object Base {
  class Student(val name: String, val shot_percent: Int, val height: Int) extends Ordered[Student]{
  
  def compare(that: Student) = {
    val shot = this.shot_percent - that.shot_percent
    if (shot == 0) this.height - that.height else shot
  }
 
  override def toString() = {
    "\nName is: " + this.name + 
    "\nShot Percentage is: " + this.shot_percent +
    "\nHeight is: " + this.height
  }
}
  def main(args: Array[String]) {
    // Input Reading
    val input_name = "basketball_game_example_input.txt"
    val input = Source.fromFile(input_name).getLines.toArray
    val num_of_cases: Int = input.head.toInt

    @ tailrec
    def splitter(inArr: Array[String]): Unit =
      // Splits input string array into individual cases
      if (inArr.isEmpty) println
      else{
        val header = inArr.head.split(" ").map(_.toInt)
        val N = header(0)
        val M = header(1)
        val P = header(2)

        val studentsStr = inArr.slice(1, N+1)
        val students = (for {
          str <- studentsStr
          strList = str.split(" ")} yield {
          new Student(strList(0), strList(1).toInt, strList(2).toInt)
        }).sortWith((a: Student, b: Student) => (a compare b) > 0)

        gameSim(N, M, P, students)

        splitter(inArr.drop(N+1))
      }

    def gameSim(N: Int, M: Int, P: Int, students: Array[Student]): Unit = {
      // Solves one case given the header info and player info
      println(students.length)
      students.foreach(println)
    }
    splitter(input.tail)
    // Output Writing
    /*
    val yesNos = for(bool <- bools) yield (if (bool) "YES" else "NO")
    val writer = new PrintWriter(new File("output.txt"))
    for(i <- 1 to num_of_cases) writer.write("Case #" + i + ": " + yesNos(i-1) +"\n")
    writer.close()*/
  }
}