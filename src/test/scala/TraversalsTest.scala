import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class TraversalsTest extends FlatSpec with Matchers with PropertyChecks {

  import com.cj.serialization.traversals._

  behavior of "SequenceListOption"

  it should "factor Option out of a List" in {
    val x: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    x.sequence shouldBe Some(List(1, 2, 3))
  }

  it should "return None if one member is None" in {
    val x: List[Option[Int]] = List(Some(1), None, Some(3))
    x.sequence shouldBe None
  }

  behavior of "SequenceMapOption"

  it should "factor Option out of a Map" in {
    val x: Map[Char, Option[Int]] = Map('a' -> Some(1), 'b' -> Some(2), 'c' -> Some(3))
    x.sequence shouldBe Some(Map('a' -> 1, 'b' -> 2, 'c' -> 3))
  }

  it should "return None if one member is None" in {
    val x: Map[Char, Option[Int]] = Map('a' -> Some(1), 'b' -> None, 'c' -> Some(3))
    x.sequence shouldBe None
  }

  behavior of "SequenceStreamOption"

  it should "factor Option out of a Stream" in {
    val x: Stream[Option[Int]] = Stream(Some(1), Some(2), Some(3))
    x.sequence shouldBe Some(Stream(1, 2, 3))
  }

  it should "return None if one member is None" in {
    val x: Stream[Option[Int]] = Stream(Some(1), None, Some(3))
    x.sequence shouldBe None
  }

  it should "play nice with infinite streams" in {
    val x: Stream[Option[Int]] = Stream(Some(1), None, Some(3))
    val inf: Stream[Option[Int]] =
      Stream.cons(Some(1),
      Stream.cons(None,
      Stream.cons(Some(3),
      Stream.from(4).map(n => Some(n)))))
    inf.sequence shouldBe None
  }

  behavior of "SequencePairOption"

  it should "factor Option out of a pair" in {
    val x: (Char, Option[Int]) = ('b', Some(2))
    x.sequence shouldBe Some(('b', 2))
  }

  it should "return None if the second member is None" in {
    val x: (Char, Option[Int]) = ('b', None)
    x.sequence shouldBe None
  }
}
