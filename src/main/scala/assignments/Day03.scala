package assignments

import scala.io.Source
import scala.annotation.tailrec
import lib.GridExtensions._
import lib.Points.Point

object Day03:

  def isGood(xs: List[Point]) =
    xs.forall(_.surroundings.forall(s => grid(s).isDigit || grid(s) == '.'))

  def getNumbers(row: List[Point]): List[List[Point]] =
    @tailrec
    def helper(
        xs: List[Point],
        curr: Option[List[Point]],
        acc: List[List[Point]]
    ): List[List[Point]] =
      xs match
        case h :: i :: t if grid(h).isDigit && !grid(i).isDigit && !curr.isDefined =>
          helper(t, None, acc :+ List(h)) // single digit
        case h :: i :: Nil if grid(i).isDigit && curr.isDefined => // eol case
          acc :+ (curr.get :+ h :+ i)
        case h :: i :: t if grid(h).isDigit && !grid(i).isDigit => // end of number
          helper(t, None, acc :+ (curr.get :+ h))
        case h :: t if grid(h).isDigit && curr.isDefined => // continuation of number
          helper(t, Option(curr.get :+ h), acc)
        case h :: t if grid(h).isDigit => // new number
          helper(t, Option(List(h)), acc)
        case _ :: t => helper(t, None, acc)
        case _      => acc
    helper(row, None, Nil)

  val grid: Grid[Char] =
    Source.fromResource("day03.txt").mkString.toList.toGrid.withDefaultValue('.')
  val numbers: List[List[Point]] = grid
    .groupBy(_._1.y)
    .values
    .map(_.toList.sortBy(_._1.x))
    .map(_.map(_._1))
    .toList
    .flatMap(getNumbers)

  def partOne(): Int =
    numbers.filterNot(isGood(_)).map(_.map(grid(_)).mkString.toInt).sum

  def partTwo(): Int = grid.keys
    .filter(grid(_) == '*')
    .filter(_.surroundings.count(grid(_).isDigit) >= 2)
    .map(g =>
      numbers
        .filter(_.flatMap(_.surroundings).contains(g))
        .map(_.map(grid(_)).mkString.toInt)
    )
    .filter(_.length == 2)
    .map(p => p.head * p.last)
    .sum
