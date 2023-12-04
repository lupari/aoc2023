package assignments

import scala.io.Source
import scala.annotation.tailrec

object Day04:
  case class Card(winning: Set[Int], actual: Set[Int]):
    def wins: Int  = (winning & actual).size
    def score: Int = if wins == 0 then 0 else math.pow(2, wins - 1).toInt

  def countWins(cards: List[Card]): Int =
    @tailrec
    def helper(xs: List[(Card, Int)], acc: Int): Int = xs match
      case (card, count) :: t =>
        val xs2 = (0 until card.wins).foldLeft(t)((cs, i) =>
          val (c, n) = cs(i)
          cs.updated(i, (c, n + count))
        )
        helper(xs2, acc + count)
      case _ => acc

    helper(cards.map(_ -> 1), 0)

  def parse(s: String): Card = s match
    case s"Card $i: $winning | $cards" =>
      val wins   = winning.trim.split(" +").map(_.toInt).toSet
      val actual = cards.trim.split(" +").map(_.toInt).toSet
      Card(wins, actual)

  val cards: List[Card] = Source.fromResource("day04.txt").getLines.map(parse).toList

  def partOne(): Int = cards.map(_.score).sum
  def partTwo(): Int = countWins(cards)
