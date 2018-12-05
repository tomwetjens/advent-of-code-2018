import scala.io.Source

object NoMatterHowYouSliceIt {

  def parse(line: String): Claim = {
    val parts = line.split("[^\\d]+")
    Claim(parts(1), Rect(parts(2).toInt, parts(3).toInt, parts(4).toInt, parts(5).toInt))
  }

  def claimRects(rects: List[Rect]): Claimed = {
    rects.foldLeft(Map.empty[(Int, Int), Int]) { (fabric, rect) => claimRect(fabric, rect) }
  }

  def claimRect(claimed: Claimed, rect: Rect): Claimed = {
    rect match {
      case Rect(_, _, 0, _) => claimed
      case Rect(x, y, width, height) => claimRect(claimColumn(claimed, x + width, y, height), Rect(x, y, width - 1, height))
    }
  }

  def claimColumn(claimed: Claimed, x: Int, y: Int, height: Int): Claimed = {
    height match {
      case 0 => claimed
      case _ => claimColumn(claim(claimed, x, y), x, y + 1, height - 1)
    }
  }

  def claim(claimed: Claimed, x: Int, y: Int): Claimed = {
    claimed + (((x, y), claimed.getOrElse((x, y), 0) + 1))
  }

  def overlap(a: Rect, b: Rect): Boolean = {
    !(a.x + a.width <= b.x || b.x + b.width < a.x || a.y + a.height <= b.y || b.y + b.height <= a.y)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("3.txt").getLines().toList
    val claims = input.map(parse)

    println(claimRects(claims.map(_.rect))
      .values
      .count(v => v >= 2))

    println(claims.filter(a => !claims.exists(b => a != b && overlap(a.rect, b.rect))))
  }

  type Claimed = Map[(Int, Int), Int]

  case class Claim(id: String, rect: Rect)

  case class Rect(x: Int, y: Int, width: Int, height: Int)

}
