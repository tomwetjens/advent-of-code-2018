import scala.io.Source

object ChronalCalibration {

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("1.txt").getLines().map(_.toInt).toList
    println(calibrate(input, 0, input, Set.empty))
  }

  def calibrate(input: List[Int], freq: Int, changes: List[Int], freqs: Set[Int]): Int = {
    if (freqs.contains(freq)) freq
    else changes match {
      case d :: rest => calibrate(input, freq + d, rest, freqs + freq)
      case Nil => calibrate(input, freq, input /*repeat*/ , freqs)
    }
  }
}