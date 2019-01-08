import scala.collection.{immutable, mutable}
import scala.io.Source

object SubterraneanSustainability {

  case class Note(pattern: Int, result: Boolean)

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("12.txt").getLines().toList

    val offset = 2

    val initial = input.head.split(" ").last.toCharArray
      .zipWithIndex
      .filter(_._1 == '#')
      .map(_._2 + offset)
      .to[immutable.TreeSet]

    val notes = input.tail
      .filter(line => line.contains("=>"))
      .map(parse)
      .filter(note => (note.pattern & 4) != (if (note.result) 1 else 0)) // does note actually change something?
      .map(note => (note.pattern, note))
      .toMap

    // part 1
    println(simulate(initial.to[mutable.TreeSet], offset, notes, 20))
    println

    // part 2
    val part2 = initial.to[mutable.TreeSet]
    val a = simulate(part2, offset, notes, 99)
    val b = generation(part2, offset, notes)
    println(b + (50000000000L - 100) * (b - a))
  }

  def parse(line: String): Note = {
    val parts = line.split(" => ")
    Note(parts(0).toCharArray
      .reverseIterator
      .zipWithIndex
      .foldLeft(0) { (result, pair) =>
        result | (if (pair._1 == '#') 1 else 0) << pair._2
      }
      .toByte, parts(1).last == '#')
  }

  def apply(state: mutable.SortedSet[Int], offset: Int, index: Int, pattern: Int, notes: Map[Int, Note]): Unit = {
    notes.get(pattern & 31).foreach { note =>
      if (note.result) state.add(offset + index)
      else state.remove(offset + index)
    }
  }

  def generation(state: mutable.SortedSet[Int], offset: Int, notes: Map[Int, Note]): Long = {
    val initial = state.clone()

    val first = state.head - offset
    val last = state.last - offset

    // Remember last 5 pots to reduce lookups in set
    var pattern = 0

    (first to last + 2).foreach { i =>
      pattern = (pattern << 1) | initial.contains(offset + i).toInt

      apply(state, offset, i - 2, pattern, notes)
    }

    // Look right of the last pot with plants
    apply(state, offset, last + 1, pattern << 1, notes)
    apply(state, offset, last + 2, pattern << 2, notes)

    state.sum - (state.size.toLong * offset.toLong)
  }

  def simulate(state: mutable.SortedSet[Int], offset: Int, notes: Map[Int, Note], n: Int): Long = {
    (1 to n).foldLeft(0L) { (_, i) =>
      print(s"${i.toString.padTo(3, ' ')}: ")
      printState(state, offset)
      println()

      generation(state, offset, notes)
    }
  }

  def printState(state: collection.Set[Int], offset: Int, current: Option[Int] = None): Unit = {
    (-offset to 200).foreach(i => {
      print(if (state.contains(offset + i)) '#' else '.')
    })
    if (current.nonEmpty) {
      println
      (-offset to 200).foreach(i => {
        print(if (i == current.get) s"^=$i" else ' ')
      })
    }
  }

  implicit def bool2int(b: Boolean): Int =  if (b) 1 else 0
}