import scala.io.Source

object TheStarsAlign {

  val maxIterations = 100000

  val letterWidth = 6
  val letterHeight = 9
  val letterSpacing = 2

  val letters = Map(
    ("#####.\n#....#\n#....#\n#....#\n#####.\n#....#\n#....#\n#....#\n#....#\n#####.", 'B'),
    (".####.\n#....#\n#.....\n#.....\n#.....\n#..###\n#....#\n#....#\n#...##\n.###.#", 'G'),
    ("#....#\n#....#\n#....#\n#....#\n######\n#....#\n#....#\n#....#\n#....#\n#....#", 'H'),
    ("#....#\n#...#.\n#..#..\n#.#...\n##....\n##....\n#.#...\n#..#..\n#...#.\n#....#", 'K'),
    ("#####.\n#....#\n#....#\n#....#\n#####.\n#.....\n#.....\n#.....\n#.....\n#.....", 'P'),
    ("#####.\n#....#\n#....#\n#....#\n#####.\n#..#..\n#...#.\n#...#.\n#....#\n#....#", 'R')
  )

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

  def message(points: List[Point], seconds: Int = 0): String = {
    if (seconds >= maxIterations) // some safe limit
      throw new Exception("No message found")

    val bb = boundingBox(points)

    if (bb.height == letterHeight) {
      println(s"Possible message at $seconds seconds")

      printPoints(points)

      ocr(points)
    } else message(points.map(move), seconds + 1)
  }

  def ocr(points: List[Point]): String = {
    val bb = boundingBox(points)

    val letterCount = (bb.width + letterSpacing) / (letterWidth + letterSpacing)

    (0 to letterCount)
      .map { i =>
        val x1 = bb.x1 + i * (letterWidth + letterSpacing)
        val lbb = BoundingBox(x1, bb.y1, x1 + letterWidth - 1, bb.y2)

        val str = (lbb.y1 to lbb.y2).foldLeft("") { (str, y) =>
          str + (lbb.x1 to lbb.x2).foldLeft("") { (line, x) =>
            if (points.exists(p => p.x == x && p.y == y)) line + "#"
            else line + "."
          } + "\n"
        }

        letters.getOrElse(str.trim(), '?')
      }
      .mkString("")
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
    println(message(points))
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
