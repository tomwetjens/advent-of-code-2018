import scala.collection.mutable

object ChronalCharge {

  def power(serial: Int, x: Int, y: Int): Int = {
    val rackID = x + 10
    (((rackID * y + serial) * rackID / 100) % 10) - 5
  }

  def square(serial: Int, spec: SquareSpec, cache: mutable.Map[SquareSpec, Square]): Square = {
    cache.getOrElseUpdate(spec,
      Square(spec,
        if (spec.size == 1) power(serial, spec.x, spec.y)
        else square(serial, SquareSpec(spec.x, spec.y, spec.size - 1), cache).power +
          (spec.x until spec.x + spec.size).map(xc => square(serial, SquareSpec(xc, spec.y + spec.size - 1, 1), cache).power).sum +
          (spec.y until spec.y + spec.size - 1).map(yc => square(serial, SquareSpec(spec.x + spec.size - 1, yc, 1), cache).power).sum))
  }

  case class SquareSpec(x: Int, y: Int, size: Int)

  case class Square(spec: SquareSpec, power: Int)

  def largest(serial: Int, minSize: Int, maxSize: Int): Square = {
    val cache = mutable.Map[SquareSpec, Square]()

    (minSize to maxSize)
      .flatMap(size => (1 to 300 - size + 1)
        .flatMap(x => (1 to 300 - size + 1)
          .map(y => square(serial, SquareSpec(x, y, size), cache))))
      .maxBy(_.power)
  }

  def main(args: Array[String]): Unit = {
    val input = 6392

    // part 1
    println(largest(input, 3, 3))

    // part 2
    println(largest(input, 1, 300))
  }

}
