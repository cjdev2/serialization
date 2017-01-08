import org.scalatest.{FlatSpec, Matchers}

class AvroTest extends FlatSpec with Matchers {

  import com.cj.serialization._
  import com.cj.serialization.avro._
  import local.test.serialization.avro.TestRecord

  "AvroDemo" should "not be out of date" in {
    AvroDemo.main(args = Array[String]())
  }

  behavior of "SerializableSpecificRecord"

  it should "be accessible for TestRecord" in {
    // given: a `TestRecord`
    val record = new TestRecord("Pokémon", 0l)

    // when: we serialize it
    serialize(record)

    // then: we should make it to the end
    true
  }

  it should "be reusable" in {
    // given: a bunch of `TestRecord`s
    val records = for {
      s <- 'a'.to('z').map(_.toString)
      n <- 0l.to(9l)
    } yield new TestRecord(s, n)

    // when: we serialize them
    records.map(serialize[TestRecord])

    // then: we should make it to then end
    true
  }

  it should "be usable concurrently" in {
    // given: a bunch of `TestRecord`s
    val records = for {
      s <- 'a'.to('z').map(_.toString)
      n <- 0l.to(9l)
    } yield new TestRecord(s, n)

    // when: we serialize them concurrently
    val strictResult = records.map(serialize[TestRecord])
    val concurrentResult = records.par.map(serialize[TestRecord])

    strictResult
      .zip(concurrentResult)
      .map({ case (z1, z2) => z1 sameElements z2 })
      .reduce(_ && _) should be (true)
  }

  it should "create output readable by other Avro clients" in {
    // given: a bunch of serialized `TestRecord`s
    val records = for {
      s <- 'a'.to('z').map(_.toString)
      n <- 0l.to(9l)
    } yield new TestRecord(s, n)
    val serializedRecords = records.map(serialize[TestRecord])

    // when: another Avro client deserializes them
    val results = serializedRecords.map(
      local.test.serialization.avro.MinimalAvroDeserializer.deserialize
    )

    // then
    results should be(records)
  }

  behavior of "AvroDeserializable[T]"

  it should "be able to create a `Deserializable[TestRecord]`" in {
    // given: a serialized `TestRecord`
    val bytes = serialize(new TestRecord("", 0l))

    // when: we provide an `AvroDeserializable[TestRecord]`
    implicit object DeserializableTestRecord
      extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

    // then: we should be able to deserialize `TestRecord`s
    deserialize(bytes)
  }

  it should "be reversible" in {
    // given: a `TestRecord` and an `AvroDeserializable[TestRecord]`
    val record = new TestRecord("Cañon", 0l)
    implicit object DeserializableTestRecord
      extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

    // when: we deserialize a serialized record
    val result = deserialize(serialize(record))

    // then: the record should survive the round trip
    result.get should be(record)
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
    // given: a bunch of serialized `TestRecord`s
    val records = for {
      s <- 'a'.to('z').map(_.toString)
      n <- 0l.to(9l)
    } yield new TestRecord(s, n)
    val serializedRecords = records.map(serialize[TestRecord])

    // when: we deserialize them concurrently
    implicit object DeserializableTestRecord
      extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

    val strictResult = serializedRecords.map(deserialize[TestRecord])
    val concurrentResult = serializedRecords.par.map(deserialize[TestRecord])

    // then: the results should be the same
    strictResult
      .zip(concurrentResult)
      .map({ case (r1, r2) => r1 == r2 })
      .reduce(_ && _) should be(true)
  }

  it should "read input created by other Avro clients" in {
    // given: a bunch of `TestRecord`s serialized by someone else
    val records = for {
      s <- 'a'.to('z').map(_.toString)
      n <- 0l.to(9l)
    } yield new TestRecord(s, n)
    val serializedRecords = records.map(
      local.test.serialization.avro.MinimalAvroSerializer.serialize
    )

    // when: we deserialize them
    implicit object DeserializableTestRecord
      extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

    val results = serializedRecords.map(deserialize[TestRecord])

    // then
    results.map(_.get) should be(records)
  }

  it should "be usable explicitly rather than implicitly" in {
    // given: some serialized records and an implicit deserializer
    val records = for {
      s <- 'a'.to('z').map(_.toString)
      n <- 0l.to(9l)
    } yield new TestRecord(s, n)
    val serializedRecords = records.map(
      local.test.serialization.avro.MinimalAvroSerializer.serialize
    )
    implicit object ImplicitDeserializer
    extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

    // when: we create a deserializer
    val deserializer: Array[Byte] => Option[TestRecord] = {
      val foo = new AvroDeserializable[TestRecord](TestRecord.getClassSchema)
      foo.deserialize
    }

    val implicitResult = serializedRecords.map(deserialize[TestRecord])
    val explicitResult = serializedRecords.par.map(deserializer)

    // then: our serializer should do all the things
    explicitResult
      .zip(implicitResult)
      .map({ case (r1, r2) => r1 == r2 })
      .reduce(_ && _) should be(true)
  }

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

  /* Legacy API */

  // TestRecord is Avro-generated java class
  // SomeRecord is native Scala class
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
