package assignments

import scala.io.Source
import scala.annotation.tailrec

object Day02:

  case class Cubes(red: Int = 0, green: Int = 0, blue: Int = 0):
    def +(c: Cubes)   = Cubes(red + c.red, green + c.green, blue + c.blue)
    def <=(c: Cubes)  = red <= c.red && green <= c.green && blue <= c.blue
    def max(c: Cubes) = Cubes(red max c.red, green max c.green, blue max c.blue)
    def power         = red * green * blue

  def parseCube(s: String): Cubes = s match
    case s"$count red"   => Cubes(red = count.toInt)
    case s"$count green" => Cubes(green = count.toInt)
    case s"$count blue"  => Cubes(blue = count.toInt)

  def parseCubes(s: String): Cubes = s.split(", ").map(parseCube).reduce(_ + _)

  def parseGame(s: String): Set[Cubes] = s match
    case s"Game $id: $cubes" => cubes.split("; ").map(parseCubes).toSet

  def sum(games: Seq[Set[Cubes]]) =
    val possible = Cubes(12, 13, 14)
    games.zipWithIndex
      .filter((g, _) => g.forall(_ <= possible))
      .map(_._2 + 1)
      .sum

  def power(games: Seq[Set[Cubes]]): Int = games.map(_.reduce(_ max _).power).sum

  val input: Seq[Set[Cubes]] = Source.fromResource("day02.txt").getLines().map(parseGame).toSeq

  def partOne(): Int = sum(input)
  def partTwo(): Int = power(input)
