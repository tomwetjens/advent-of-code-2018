import scala.io.Source

object AlchemicalReduction {

  def react(input: List[Char], result: List[Char] = List.empty): Int = {
    input match {
      case a :: remaining =>
        result match {
          case b :: tail =>
            if (a != b && a.toUpper == b.toUpper) react(remaining, tail)
            else react(remaining, a :: result)
          case Nil => react(remaining, a :: result)
        }
      case Nil => result.length
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("5.txt").getLines.toList.head.toCharArray.toList

    // part 1
    println(react(input))

    // part 2
    println(('a' to 'z').map(a => react(input.filter(b => b.toLower != a))).min)
  }

}
