import scala.io.Source

object MemoryManeuver {

  case class Node(children: List[Node], metadata: List[Int]) {

    def walk[B](z: B)(op: (B, Node) => B): B = {
      var result = op(z, this)
      children.foreach { child => result = child.walk(result)(op) }
      result
    }

    def value: Int = {
      if (children.isEmpty) metadata.sum
      else metadata
        .filter { n => n <= children.size }
        .map { n => children(n - 1).value }
        .sum
    }

  }

  def parse(numbers: Iterator[Int]): Node = {
    val childCount = numbers.next
    val metadataCount = numbers.next

    Node(
      children = (1 to childCount).map { _ => parse(numbers) }.toList,
      metadata = numbers.take(metadataCount).toList
    )
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("8.txt").getLines().toList.head
    val numbers = input.split(' ').map(_.toInt).toList

    val tree = parse(numbers.iterator)

    // part 1
    println(tree.walk(0) { (sum, node) => sum + node.metadata.sum })

    // part 2
    println(tree.value)
  }

}
