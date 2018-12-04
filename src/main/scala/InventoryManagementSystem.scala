import scala.io.Source

object InventoryManagementSystem {

  def charFrequencies(id: String): Map[Char, Int] = {
    id.groupBy(identity).mapValues(_.length)
  }

  def checksum(input: List[String]): Int = {
    case class Checksum(twos: Int, threes: Int) {
      def calculate = twos * threes
    }

    input.foldLeft(Checksum(0, 0)) { (counts, id) =>
      val cf = charFrequencies(id).values.toSet
      Checksum(
        counts.twos + (if (cf.contains(2)) 1 else 0),
        counts.threes + (if (cf.contains(3)) 1 else 0)
      )
    }.calculate
  }

  def diff(a: Array[Char], b: Array[Char], offset: Int = 0): Int = {
    if (a.length <= offset) b.length - offset
    else if (b.length <= offset) a.length - offset
    else (if (a(offset) == b(offset)) 0 else 1) + diff(a, b, offset + 1)
  }

  def similar(input: List[String]): List[String] = {
    input.filter(a => input.exists(b => diff(a.toCharArray, b.toCharArray) == 1))
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("2.txt").getLines().toList
    println(checksum(input))
    println(similar(input).reduce { (a, b) => a.intersect(b) })
  }

}
