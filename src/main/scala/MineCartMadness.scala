import scala.collection.{SortedMap, mutable}
import scala.io.Source

object MineCartMadness {

  class Cart(var x: Int,
             var y: Int,
             var dir: Char,
             var turns: Int = 0,
             var crashed: Boolean = false)

  def printTrack(track: SortedMap[(Int, Int), Char], carts: List[Cart]): Unit = {
    (0 to 150).foreach { y =>
      (0 to 150).foreach { x =>
        val c = carts
          .find(cart => cart.x == x && cart.y == y)
          .map(cart => if (cart.crashed) 'X' else cart.dir)
          .getOrElse(track.getOrElse((x, y), ' '))
        print(c)
      }
      println
    }
  }

  def tick(carts: mutable.Set[Cart], track: SortedMap[(Int, Int), Char]): Unit = {
    carts
      .toList
      .sortBy(cart => (cart.y, cart.x))
      .filter(cart => !cart.crashed)
      .foreach { cart =>
        cart.dir match {
          case '<' => cart.x -= 1
          case '>' => cart.x += 1
          case '^' => cart.y -= 1
          case 'v' => cart.y += 1
        }

        val crashedWith = carts.find(other => other != cart && other.x == cart.x && other.y == cart.y)
        if (crashedWith.nonEmpty) {
          cart.crashed = true
          crashedWith.get.crashed = true

          carts.remove(cart)
          carts.remove(crashedWith.get)
        } else {
          val c = track((cart.x, cart.y))

          cart.dir = c match {
            case '/' => cart.dir match {
              case '^' => '>'
              case 'v' => '<'
              case '>' => '^'
              case '<' => 'v'
            }
            case '\\' => cart.dir match {
              case '^' => '<'
              case 'v' => '>'
              case '>' => 'v'
              case '<' => '^'
            }
            case '+' => {
              cart.turns += 1

              (cart.turns - 1) % 3 match {
                case 0 => cart.dir match { // turn left
                  case '^' => '<'
                  case 'v' => '>'
                  case '<' => 'v'
                  case '>' => '^'
                }
                case 1 => cart.dir // go straight
                case 2 => cart.dir match { // turn right
                  case '^' => '>'
                  case 'v' => '<'
                  case '<' => '^'
                  case '>' => 'v'
                }
              }
            }
            case _ => cart.dir
          }
        }
      }
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("13.txt").getLines

    val cells = lines
      .zipWithIndex
      .flatMap { case (line, y) =>
        line.toCharArray
          .zipWithIndex
          .filter(_._1 != ' ')
          .map { case (cell, x) => ((x, y), cell) }
      }
      .toMap

    val carts = cells
      .filter { case (_, c) => c == '>' || c == '<' || c == 'v' || c == '^' }
      .map { case (coord, dir) =>
        val (x, y) = coord
        new Cart(x, y, dir)
      }
      .toList

    val track: SortedMap[(Int, Int), Char] = SortedMap[(Int, Int), Char]() ++ cells
      .map { case (coord, c) =>
        val (x, y) = coord
        (coord, c match {
          case '>' | '<' | '^' | 'v' =>
            val left = cells.getOrElse((x - 1, y), ' ')
            val right = cells.getOrElse((x + 1, y), ' ')
            val above = cells.getOrElse((x, y - 1), ' ')
            val below = cells.getOrElse((x, y + 1), ' ')

            // complete the track, as if there were no cart
            if (left != ' ' && left != '|') {
              if (right != ' ' && right != '|') {
                if (above != ' ' && above != '-' && below != ' ' && below != '-') '+'
                else '-'
              } else {
                if (above != ' ' && above != '-') '/'
                else '\\'
              }
            } else {
              if (right != ' ' && right != '|') {
                if (above != ' ' && above != '-') '\\'
                else '/'
              } else '|'
            }
          case _ => c
        })
      }

    part1(carts, track)

    part2(carts.to[mutable.Set], track)
  }

  private def part1(carts: List[Cart], track: SortedMap[(Int, Int), Char]) = {
    do {
      tick(carts.to[mutable.Set], track)
    } while (!carts.exists(cart => cart.crashed))

    carts.filter(cart => cart.crashed).foreach(cart => println(s"${cart.x},${cart.y}"))
  }

  private def part2(carts: mutable.Set[Cart], track: SortedMap[(Int, Int), Char]) = {
    do {
      tick(carts, track)
    } while (carts.size > 1)

    carts.foreach(cart => println(s"${cart.x},${cart.y}"))
  }
}
