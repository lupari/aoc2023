import org.scalatest._
import assignments.Day04

class Test04 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day04.partOne() should be(21959)
    Day04.partTwo() should be(5132675)
  }
