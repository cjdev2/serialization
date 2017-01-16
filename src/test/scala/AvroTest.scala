import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class AvroTest extends FlatSpec with Matchers with PropertyChecks {

  import com.cj.serialization._
  import com.cj.serialization.avro._
  import local.test.serialization.avro.TestRecord

  "AvroDemo" should "not be out of date" in {
    AvroDemo.main(args = Array[String]())
  }

  behavior of "SerializableSpecificRecord"

  it should "serialize `TestRecord`s, never throw, never return null" in {
    forAll { (s: String, n: Long) =>
      // given
      val record = new TestRecord(s, n)

      // when
      val threwException = Try(serialize(record)).isFailure
      val returnedNull = serialize(record) == null

      // then
      threwException should be(false)
      returnedNull should be(false)
    }
  }

  it should "be usable concurrently" in {
    forAll{ (pairs: List[(String, Long)]) =>
      // given
      val records = pairs.map(pair => new TestRecord(pair._1, pair._2))

      // when
      val strictResult = records.map(serialize[TestRecord])
      val concurrentResult = records.par.map(serialize[TestRecord])

      // then
      strictResult
        .zip(concurrentResult)
        .forall(results => results._1 sameElements results._2) should be(true)
    }
  }

  it should "create output readable by other Avro clients" in {
    forAll { (s: String, n: Long) =>
      // given: a record we serialized
      val record = new TestRecord(s, n)
      val serializedRecord = serialize(record)

      // when: another Avro client deserializes it
      val result = local.test.serialization.avro.MinimalAvroDeserializer
        .deserialize(serializedRecord)

      // then: the deserialized record should match the original
      result == record should be(true)
    }
  }

  behavior of "AvroDeserializable[T]"

  it should "be able to create a `Deserializable[TestRecord]`" in {
    forAll { (s: String, n: Long) =>
      // given: a serialized `TestRecord`
      val record = new TestRecord(s, n)
      val bytes = serialize(record)

      // when: we create an `AvroDeserializable[TestRecord]`
      implicit object DeserializableTestRecord
        extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

      // then: we should be able to deserialize `TestRecord`s
      deserialize(bytes).contains(record) should be(true)
    }
  }

  it should "never throw or return null when deserializing" in {
    forAll { (bs: Array[Byte]) =>
      // given: a `Deserializable[TestRecord]`
      implicit object DeserializableTestRecord
        extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

      // when: we deserialize a byte array
      val threwException = Try(deserialize[TestRecord](bs)).isFailure
      val returnedNull = deserialize[TestRecord](bs).contains(null)

      // then: we should not have thrown or returned a null
      threwException should be(false)
      returnedNull should be(false)
    }
  }

  it should "satisfy the serializer contract" in {
    // given
    implicit object DeserializableTestRecord
      extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

    // starting from a TestRecord
    forAll { (s: String, n: Long) =>
      val record = new TestRecord(s, n)
      val left1 = deserialize[TestRecord](serialize(record))
      val right1 = Some(record)
      left1 == right1 should be(true)
    }

    // starting from an Array[Byte]
    forAll { (bs: Array[Byte]) =>
      val left2 = deserialize[TestRecord](bs)
        .map(serialize[TestRecord])
        .flatMap(deserialize[TestRecord])
      val right2 = deserialize[TestRecord](bs)
      left2 == right2 should be(true)
    }
  }

  it should "return `None` when given bad input" in {
    // given: some bad bytes
    val badBytes = "baaad".toCharArray.map(_.toByte)
    implicit object DeserializableTestRecord
      extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

    // when: we serialize them
    val result = deserialize(badBytes)

    // then: the result should be `None`
    result should be(None)
  }

  it should "be usable concurrently" in {
    forAll { (pairs: List[(String, Long)]) =>
      // given: a bunch of serialized `TestRecord`s and a deserializer
      val records = pairs.map(pair => new TestRecord(pair._1, pair._2))
      val serializedRecords = records.map(serialize[TestRecord])
      implicit object DeserializableTestRecord
        extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

      // when: we deserialize them strictly and concurrently
      val strictResult = serializedRecords.map(deserialize[TestRecord])
      val concurrentResult = serializedRecords.par.map(deserialize[TestRecord])

      // then: the results should be the same
      strictResult
        .zip(concurrentResult)
        .forall(results => results._1 == results._2) should be(true)
    }
  }

  it should "read input created by other Avro clients" in {
    forAll { (s: String, n: Long) =>
      // given: a `TestRecord` serialized by someone else
      val record = new TestRecord(s, n)
      val serializedRecord =
        local.test.serialization.avro.MinimalAvroSerializer.serialize(record)
      implicit object DeserializableTestRecord
        extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

      // when: we deserialize them
      val result = deserialize[TestRecord](serializedRecord)

      // then: the deserialization should be faithful
      result.contains(record) should be(true)
    }
  }

  it should "be usable explicitly" in {
    forAll { (s: String, n: Long) =>
      // given: some serialized records and an implicit deserializer
      val record = new TestRecord(s, n)
      val serializedRecord =
        local.test.serialization.avro.MinimalAvroSerializer.serialize(record)
      implicit object ImplicitDeserializer
        extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

      // when: we create a deserializer and use it to deserialize
      object TestRecordD
        extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

      val implicitResult = deserialize[TestRecord](serializedRecord)
      val explicitResult = TestRecordD.deserialize(serializedRecord)

      // then: its result should match the implicit deserializer's
      explicitResult == implicitResult should be(true)
    }
  }

  /* Legacy API */

  behavior of "makeAvroDeserializer"

  it should "provide an alternative API for deserializing Avro" in {
    // given: a `TestRecord`
    val record = new TestRecord("", 0l)

    // when: we invoke `makeAvroDeserializer`
    val testRecordDeserializer =
      makeAvroDeserializer[TestRecord](TestRecord.getClassSchema)

    // then: the resulting deserializer should behave as expected
    testRecordDeserializer(serialize(record)).get should be(record)
  }

  // `TestRecord` is an Avro-generated java class
  // `SomeRecord` is a native Scala class
  case class SomeRecord(imAString: String, imALong: Long)

  behavior of "TestRecord"

  it should "survive serialization-deserialization" in {
    // given: a TestRecord
    val record = new TestRecord("foo", 1l)

    // when: it is serialized and then deserialized
    val serializer: RecordSerializer[TestRecord] =
      mkAvroSerializer[TestRecord]()
    val deserializer: RecordDeserializer[TestRecord] =
      mkAvroDeserializer(TestRecord.getClassSchema)
    val foo = serializer(record)
    val s = foo.toString

    val result: TestRecord = deserializer(foo)

    // then: the result should be the same as the starting record
    result should be(record)
  }

  it should "survive serialization-deserialization when reusing (de)serializer" in {
    // given: a TestRecord
    val record1 = new TestRecord("foo", 1l)
    val record2 = new TestRecord("foo2", 2l)
    val serializer: RecordSerializer[TestRecord] =
      mkAvroSerializer[TestRecord]()
    val deserializer: RecordDeserializer[TestRecord] =
      mkAvroDeserializer(TestRecord.getClassSchema)

    // when: it is serialized and then deserialized
    val foo1 = serializer(record1)
    val result1: TestRecord = deserializer(foo1)
    val foo2 = serializer(record2)
    val result2: TestRecord = deserializer(foo2)

    // then: the result should be the same as the starting record
    result1 should be(record1)
    result2 should be(record2)
  }

  behavior of "SomeRecord"

  it should "survive encoding-decoding" in {
    // given: a SomeRecord
    val record = SomeRecord("foo", 1l)
    def someRecordToAvro: SomeRecord => TestRecord = {
      case SomeRecord(str, lng) => new TestRecord(str, lng)
    }
    def someRecordFromAvro: TestRecord => SomeRecord = {
      avro => SomeRecord(avro.getFoo, avro.getBar)
    }
    val encode = mkSerializer(someRecordToAvro)
    val decode = mkDeserializer(someRecordFromAvro, TestRecord.getClassSchema)

    //when: it is encoded and then decoded
    val result = decode(encode(record))

    //then: we get back what we started with
    result should be(Some(record))
  }

  behavior of "mkAvroDeserializer"

  it should "return a deserializer that throws if deserialization fails" in {
    // given: some bad bytes
    val badBytes = "baaaad".toCharArray.map(_.toByte)
    val deserializer = mkAvroDeserializer[TestRecord](TestRecord.getClassSchema)

    // when
    val result = scala.util.Try(deserializer(badBytes))

    // then
    result.failed.get.getMessage should be("mkAvroDeserializer: Failed to parse baaaad as Avro class local.test.serialization.avro.TestRecord")
  }
}
