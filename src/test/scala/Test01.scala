import org.scalatest._
import assignments.Day01

class Test01 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day01.partOne() should be(54338)
    Day01.partTwo() should be(53389)
  }
