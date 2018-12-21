import scala.io.Source

object SumOfItsParts {

  case class Edge(from: Char, to: Char)

  def parse(line: String): Edge = {
    val parts = line.split(" ")
    Edge(parts(1).charAt(0), parts(7).charAt(0))
  }

  def visit(current: Set[Char], edges: Set[Edge], result: List[Char] = List.empty): List[Char] = {
    current.toList
      .filter(n => !edges.exists(e => e.to == n))
      .sorted match {
      case head :: tail => {
        val outgoing = edges.filter { edge => edge.from == head }

        visit(tail.toSet | outgoing.map(_.to), edges -- outgoing, head :: result)
      }
      case Nil => result
    }
  }

  case class Work(node: Char, finished: Int)

  def visit2(time: Int, workers: Map[Int, Option[Work]], available: Set[Char], edges: Set[Edge]): Int = {
    if (!workers.values.exists(_.nonEmpty) && edges.isEmpty) {
      time
    } else {
      val availableWorkers = workers
        .mapValues(f => f.flatMap(work => if (work.finished == time) None else Some(work)))
        .filter { case (_, work) => work.isEmpty }
        .keys

      availableWorkers match {
          // all workers still busy, so just advance time
        case Nil => visit2(time + 1, workers, available, edges)
        case _ =>
          val finishedNodes: List[Char] = workers
            .values
            .flatten
            .filter(w => w.finished == time)
            .map(_.node)
            .toList

          val outgoing = finishedNodes.flatMap { n => edges.filter { edge => edge.from == n } }.toSet
          val newAvailable = available | outgoing.map(_.to)
          val newEdges = edges -- outgoing

          val nodesToStart = newAvailable
            .filter(n => !newEdges.exists(e => e.to == n))
            .toList
            .sorted
            .take(availableWorkers.size)

          val workToAssign = nodesToStart.map { n => Work(n, time + 60 + n - 'A' + 1) }

          val assignedWorkers = workers
            .mapValues(f => f.flatMap(work => if (work.finished == time) None else Some(work))) ++ availableWorkers.zip(workToAssign.map(w => Some(w)))

          visit2(time + 1, assignedWorkers, newAvailable -- nodesToStart, newEdges)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("7.txt").getLines().toList

    val edges = input.map(parse).toSet

    val incoming = edges.groupBy(_.to)
    val outgoing = edges.groupBy(_.from)
    val starts = outgoing.keySet.diff(incoming.keySet)

    // part 1
    println(visit(starts, edges).reverse.mkString(""))

    // part 2
    val workers = 5
    // assuming here there are enough workers to assign all start nodes to
    val initialWorkers = (1 to workers).map(i => (i, None)).toMap ++ (1 to workers).zip(starts.map { n => Some(Work(n, 60 + n - 'A')) })
    println(visit2(0, initialWorkers, Set.empty, edges))
    // 1163
  }

}
