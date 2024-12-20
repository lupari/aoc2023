package assignments

import scala.io.Source
import scala.annotation.tailrec

object Day05:

  case class Vent(x1: Int, y1: Int, x2: Int, y2: Int):
    val dx                  = math.signum(x2 - x1)
    val dy                  = math.signum(y2 - y1)
    def isDiagonal: Boolean = dx * dy != 0
    def points: Seq[(Int, Int)] = Range
      .inclusive(0, math.max(math.abs(x2 - x1), math.abs(y2 - y1)))
      .map(step => (x1 + dx * step, y1 + dy * step))

  val regex = """(\d+),(\d+) -> (\d+),(\d+)""".r
  val vents = input.map { case regex(x0, y0, x1, y1) =>
    Vent(x0.toInt, y0.toInt, x1.toInt, y1.toInt)
  }

  def countOverlaps(vs: List[Vent]) =
    vs.flatMap(_.points).groupBy(identity).values.count(_.size > 1)

  val input: List[String] = Source.fromResource("day05.txt").getLines.toList

  def partOne(): Int = countOverlaps(vents.filterNot(_.isDiagonal))
  
