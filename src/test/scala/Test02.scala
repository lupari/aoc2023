import org.scalatest._
import assignments.Day02

class Test02 extends flatspec.AnyFlatSpec with matchers.should.Matchers:

  it should "calculate correct result" in {
    Day02.partOne() should be(2528)
    Day02.partTwo() should be(67363)
  }
