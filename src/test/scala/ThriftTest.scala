import org.scalatest.{FlatSpec, Matchers}

class ThriftTest extends FlatSpec with Matchers {

  import com.cj.serialization._
  import com.cj.serialization.thrift._
  import local.test.serialization.thrift.scala.TestRecord

  "ThriftDemo" should "not be out of date" in {
    ThriftDemo.main(args = Array[String]())
  }

  behavior of "ThriftSerializer"

  it should "be accessible for TestRecord" in {
    // given: a `TestRecord`
    val record = TestRecord("", 0l)

    // when: the software is written

    // then: we should make it to the end
    serialize(record)
  }

  behavior of "ThriftDeserializer"

  it should "be able to create a `Deserializable[TestRecord]`" in {
    // given: a serialized `TestRecord`
    val bytes = serialize(TestRecord("", 0l))

    // when: we provide a `ThriftDeserializer[TestRecord]`
    implicit object DeserializableTestRecord
      extends ThriftDeserializer[TestRecord](TestRecord)

    // then: we should be able to deserialize `TestRecord`s
    deserialize[TestRecord](bytes)
  }

  it should "be reversible" in {
    // given: a `TestRecord` and a `ThriftDeserializer[TestRecord]`
    val record = TestRecord("", 0l)
    implicit object DeserializableTestRecord
      extends ThriftDeserializer[TestRecord](TestRecord)

    // when: we deserialize a serialized record
    val result: Option[TestRecord] = deserialize(serialize(record))

    // then: the record should survive the round trip
    result should be(Some(record))
  }

  it should "return `None` when given bad input" in {
    // given: some bad bytes
    val badBytes = "baaad".toCharArray.map(_.toByte)
    implicit object DeserializableTestRecord
      extends ThriftDeserializer[TestRecord](TestRecord)

    // when: we serialize them
    val result: Option[TestRecord] = deserialize(badBytes)

    // then: the result should be `None`
    result should be(None)
  }

  // TODO: Test guarantees on coherence (no nulls)
  // TODO: Test non-ascii characters
  // TODO: Test contract satisfaction
  // TODO: Test concurrency/thread-safety
  // TODO: Test cross-platform compatibility
}
