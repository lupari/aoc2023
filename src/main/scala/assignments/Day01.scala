package assignments

import scala.io.Source
import scala.annotation.tailrec

object Day01:

  def numerify(line: String): String =
    val numbers =
      Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine").zipWithIndex

    @tailrec
    def helper(xs: List[Char], acc: String): String = xs match
      case h :: t =>
        val trial = acc :+ h
        numbers.find(trial.contains(_._1)) match
          case Some((n, i)) => helper(t, trial.replace(n, (i + 1).toString) :+ h)
          case _            => helper(t, trial)
      case Nil => acc

    helper(line.toList, "")

  def calibrate(xs: List[String])(fn: String => String): Int =
    xs.map(fn).map(_.filter(_.isDigit).map(_.asDigit)).map(i => i.head * 10 + i.last).sum

  val input: List[String] = Source.fromResource("day01.txt").getLines.toList

  def partOne(): Int = calibrate(input)(identity)
  def partTwo(): Int = calibrate(input)(numerify)
