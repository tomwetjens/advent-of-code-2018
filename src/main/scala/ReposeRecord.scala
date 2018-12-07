import java.time.{Duration, LocalDateTime}

import scala.io.Source

object ReposeRecord {

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("4.txt").getLines().toList
    val records = parse(input)

    val periodsAsleepPerGuard = periodsAsleep(records)

    println(strategy1(periodsAsleepPerGuard))

    println(strategy2(periodsAsleepPerGuard))
  }

  def strategy1(periodsAsleepPerGuard: Map[Int, List[Period]]): Int = {
    val mostAsleep = periodsAsleepPerGuard.maxBy { case (_, periods) => periods.map(period => Duration.between(period.from, period.to).toMinutes).sum }

    val minuteMostAsleep = countAsleepPerMinute(mostAsleep._2)
      .zipWithIndex
      .maxBy(_._1)
      ._2

    mostAsleep._1 * minuteMostAsleep
  }

  def strategy2(periodsAsleepPerGuard: Map[Int, List[Period]]): Int = {
    val mostAsleepGuardAtSameMinute = periodsAsleepPerGuard
      .mapValues { periods => countAsleepPerMinute(periods).zipWithIndex.maxBy(_._1) }
      .maxBy { case (_, minuteMostAsleep) => minuteMostAsleep._1 }

    mostAsleepGuardAtSameMinute._1 * mostAsleepGuardAtSameMinute._2._2
  }

  def countAsleepPerMinute(periodsMostAsleep: List[Period]): Array[Int] = {
    periodsMostAsleep
      .foldLeft(Array.fill(60)(0)) { (minutes, period) =>
        minutes
          .zipWithIndex
          .map { case (count, index) => count + (if (index >= period.from.getMinute && index < period.to.getMinute) 1 else 0) }
      }
  }

  def periodsAsleep(records: List[Record], startSleep: Option[Record] = None, result: Map[Int, List[Period]] = Map.empty): Map[Int, List[Period]] = {
    records match {
      case head :: tail =>
        if (head.asleep)
          startSleep match {
            case None =>
              periodsAsleep(tail, Some(head), result)
            case _ =>
              periodsAsleep(tail, startSleep, result)
          }
        else startSleep match {
          case Some(Record(from, id, _)) =>
            periodsAsleep(tail, None, result + ((id, result.getOrElse(head.id, List.empty) ::: List(Period(from, head.dateTime)))))
          case _ =>
            periodsAsleep(tail, None, result)
        }
      case _ => result
    }
  }

  def parse(input: List[String]): List[Record] = {
    input
      .sorted // sort chronologically
      .foldLeft(List.empty[Record]) { (list, line) =>
      //      [1518-11-01 00:00] Guard #10 begins shift
      //      [1518-11-01 00:05] falls asleep
      //      [1518-11-01 00:25] wakes up
      val parts = line.split("[^a-zA-Z0-9]+")

      val dateTime = LocalDateTime.of(parts(1).toInt, parts(2).toInt, parts(3).toInt, parts(4).toInt, parts(5).toInt)

      (parts(6) match {
        case "Guard" => Record(dateTime, parts(7).toInt, asleep = false)
        case "falls" => Record(dateTime, list.head.id, asleep = true)
        case "wakes" => Record(dateTime, list.head.id, asleep = false)
      }) :: list
    }
      .reverse
  }

  case class Period(from: LocalDateTime, to: LocalDateTime)

  case class Record(dateTime: LocalDateTime, id: Int, asleep: Boolean)

}
