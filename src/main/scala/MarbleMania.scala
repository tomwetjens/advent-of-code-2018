import scala.io.Source

object MarbleMania {

  class Marble(val num: Int) {
    var prev: Marble = this
    var next: Marble = this

    def this(num: Int, prev: Marble, next: Marble) = {
      this(num)
      this.prev = prev
      this.next = next
    }
  }

  case class State(current: Marble = new Marble(0), scores: Map[Int, Long] = Map.empty)

  def play(numberOfPlayers: Int, lastMarble: Int, currentPlayer: Int = 1, marble: Int = 1, state: State = State()): State = {
    if (marble > lastMarble) {
      state
    } else if (marble % 23 != 0) {
      val after = state.current.next
      val before = after.next

      // insert between
      val inserted = new Marble(marble, after, before)
      after.next = inserted
      before.prev = inserted

      play(numberOfPlayers, lastMarble, (currentPlayer % numberOfPlayers) + 1, marble + 1, State(inserted, state.scores))
    } else {
      val removed = (1 to 7).foldRight(state.current) { (_, current) => current.prev }

      removed.prev.next = removed.next

      play(numberOfPlayers, lastMarble, (currentPlayer % numberOfPlayers) + 1, marble + 1, State(removed.next,
        state.scores + ((currentPlayer, state.scores.getOrElse(currentPlayer, 0L) + marble + removed.num))))
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("9.txt").getLines().next

    val parts = input.split(" ")
    val numberOfPlayers = parts(0).toInt
    val lastMarble = parts(6).toInt

    // part 1
    println(play(numberOfPlayers, lastMarble).scores.values.max)

    // part 2
    println(play(numberOfPlayers, lastMarble * 100).scores.values.max)
  }

}
