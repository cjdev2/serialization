//import org.scalatest.{FlatSpec, Matchers}
//
//class ThriftTest extends FlatSpec with Matchers {
//
//  import com.cj.serialization._
//  import com.cj.serialization.thrift._
//  import local.test.serialization.thrift.scala.TestRecord
//
//  behavior of "SerializableThriftStruct"
//
//  it should "be accessible for TestRecord" in {
//    // given: a `TestRecord`
//    val record = TestRecord("", 0l)
//
//    // when: we serialize it
//    serialize(record)
//
//    // then: we should make it to the end
//    true
//  }
//
//  it should "be reusable" in {
//    // given: a bunch of `TestRecord`s
//    val records = for {
//      s <- 'a'.to('z').map(_.toString)
//      n <- 0l.to(9l)
//    } yield TestRecord(s, n)
//
//    // when: we serialize them
//    records.map(serialize[TestRecord])
//
//    // then: we should make it to then end
//    true
//  }
//
//  behavior of "ThriftDeserializable[T]"
//
//  it should "be able to create a `Deserializable[TestRecord]`" in {
//    // given: a serialized `TestRecord`
//    val bytes = serialize(TestRecord("", 0l))
//
//    // when: we provide a `ThriftDeserializable[TestRecord]`
//    implicit object DeserializableTestRecord
//      extends ThriftDeserializable[TestRecord]
//
//    // then: we should be able to deserialize `TestRecord`s
//    deserialize(bytes)
//  }
//
//  it should "be reversible" in {
//    // given: a `TestRecord` and a `ThriftDeserializable[TestRecord]`
//    val record = TestRecord("", 0l)
//    implicit object DeserializableTestRecord
//      extends ThriftDeserializable[TestRecord]
//
//    // when: we deserialize a serialized record
//    val result = deserialize(serialize(record))
//
//    // then: the record should survive the round trip
//    result.get should be(record)
//  }
//
//  it should "return `None` when given bad input" in {
//    // given: some bad bytes
//    val badBytes = "baaad".toCharArray.map(_.toByte)
//    implicit object DeserializableTestRecord
//      extends ThriftDeserializable[TestRecord]
//
//    // when: we serialize them
//    val result = deserialize(badBytes)
//
//    // then: the result should be `None`
//    result should be(None)
//  }
//
//  behavior of "makeThriftDeserializer"
//
//  it should "provide an alternative API for deserializing Thrift" in {
//    // given: a `TestRecord`
//    val record = TestRecord("", 0l)
//
//    // when: we invoke `makeAvroDeserializer`
//    val deserializeTestRecord =
//      makeThriftDeserializer[TestRecord]
//
//    // then: the resulting deserializer should behave as expected
//    deserializeTestRecord(serialize(record)).get should be(record)
//  }
//}
