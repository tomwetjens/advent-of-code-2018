import scala.io.Source

object ChronalCoordinates {

  // Calculate smallest rectangle that contains all points
  def boundingBox(coords: List[Coord]): BoundingBox = {
    coords.foldLeft(BoundingBox(Int.MaxValue, Int.MaxValue, Int.MinValue, Int.MinValue)) { (result, coord) =>
      BoundingBox(
        result.x1.min(coord.x),
        result.y1.min(coord.y),
        result.x2.max(coord.x + 1),
        result.y2.max(coord.y)
      )
    }
  }

  def manhattan(a: Location, b: Coord): Int = (a.x - b.x).abs + (a.y - b.y).abs

  def infinite(area: Seq[Location], bb: BoundingBox): Boolean = {
    area.exists(loc => loc.x <= bb.x1 || loc.y <= bb.y1 || loc.x >= bb.x2 || loc.y >= bb.y2)
  }

  def maxFiniteAreaSize(coords: List[Coord]): Int = {
    val bb = boundingBox(coords)

    val result: Seq[(Location, Option[Coord])] = locations(bb)
      .map(location => (location, closest(coords, location)))

    val areas: Map[Coord, Seq[Location]] = result
      .flatMap { case (location, closest) => closest.map(cl => (location, cl)) }
      .groupBy(_._2)
      .mapValues(_.map(_._1))

    val finiteAreas = areas.filter { case (_, area) => !infinite(area, bb) }

    finiteAreas
      .mapValues(_.length)
      .values
      .max
  }

  def locations(bb: BoundingBox): Seq[Location] = {
    (bb.x1 to bb.x2).flatMap(x => (bb.y1 to bb.y2).map(y => Location(x, y)))
  }

  def closest(coords: List[Coord], location: Location): Option[Coord] = {
    val candidates = coords
      .map(coord => (coord, manhattan(location, coord)))
      .filter(d => d._2 == coords
        .map(coord => (coord, manhattan(location, coord)))
        .minBy(_._2)._2)

    if (candidates.length == 1) Some(candidates.head._1)
    else None
  }

  def regionSize(coords: List[Coord]): Int = {
    val bb = boundingBox(coords)
    val locs = locations(bb)

    val result: Seq[(Location, Int)] = locs
      .map(location => (location, coords.map(coord => manhattan(location, coord)).sum))
      .filter { case (_, total) => total < 10000 }

    result.length
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("6.txt").getLines

    val coords = input.map(parse).toList

    // part 1
    println(maxFiniteAreaSize(coords))

    // part 2
    println(regionSize(coords))
  }

  case class Location(x: Int, y: Int)

  case class Coord(x: Int, y: Int)

  case class BoundingBox(x1: Int, y1: Int, x2: Int, y2: Int)

  def parse(line: String): Coord = {
    val parts = line.split(", ")
    Coord(parts(0).toInt, parts(1).toInt)
  }

}
