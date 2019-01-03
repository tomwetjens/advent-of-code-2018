import scala.io.Source

object TheStarsAlign {

  def parse(line: String): Point = {
    val parts = line.split("[<>, ]+")
    Point(parts(1).toInt, parts(2).toInt, parts(4).toInt, parts(5).toInt)
  }

  def boundingBox(points: List[Point]): BoundingBox = {
    points.foldLeft(BoundingBox(Int.MaxValue, Int.MaxValue, Int.MinValue, Int.MinValue)) { (result, point) =>
      BoundingBox(
        result.x1.min(point.x),
        result.y1.min(point.y),
        result.x2.max(point.x),
        result.y2.max(point.y)
      )
    }
  }

  def move(point: Point): Point = {
    Point(point.x + point.vx, point.y + point.vy, point.vx, point.vy)
  }

  def sequences(numbers: List[Int]): List[List[Int]] = {
    numbers
      .distinct
      .sorted
      .foldLeft(List[List[Int]]()) { (segments, n) =>
        segments match {
          case current :: finished =>
            if (current.head == n - 1) // segment continues
              (n :: current) :: finished
            else // segment ends, new segment begins
              List(n) :: segments
          case Nil => List(List(n))
        }
      }
  }

  def containsLetters(points: List[Point]): Boolean = {
    verticalLines(points).count(line => line.length > 8) >= 4 // sensible number of lines that could be letters
  }

  def verticalLines(points: List[Point]): List[Line] = {
    points
      .groupBy(_.x)
      .flatMap { case (x, column) => sequences(column.map(_.y))
        // a segment must have at least 2 elements to be a line
        .filter(segment => segment.size > 1)
        .map(segment => Line(x, segment.head - segment.size + 1, x, segment.head))
      }
      .toList
  }

  def message(points: List[Point], seconds: Int = 0): List[Point] = {
    if (seconds >= 100000) // some safe limit
      throw new Exception("No message found")

    val bb = boundingBox(points)

    // quick exit if box too large
    if (bb.width < 100 && bb.height < 100 && containsLetters(points)) {
      println(s"Possible message at $seconds seconds")
      points
    } else message(points.map(move), seconds + 1)
  }

  def printPoints(points: List[Point]): Unit = {
    val bb = boundingBox(points)

    (bb.y1 to bb.y2).foreach { y =>
      (bb.x1 to bb.x2).foreach { x =>
        if (points.exists(p => p.x == x && p.y == y)) print("#")
        else print(".")
      }
      println()
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("10.txt").getLines().toList

    val points = input.map(parse)
    printPoints(message(points))
  }

  case class Point(x: Int, y: Int, vx: Int, vy: Int)

  case class BoundingBox(x1: Int, y1: Int, x2: Int, y2: Int) {
    def width: Int = x2 - x1

    def height: Int = y2 - y1
  }

  case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
    def length: Int = Math.abs((x2 - x1) + (y2 - y1))
  }

}
