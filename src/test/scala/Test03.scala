import org.scalatest._
import assignments.Day03

class Test03 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day03.partOne() should be(517021)
    Day03.partTwo() should be(81296995)
  }
