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
    val record = TestRecord("foó", 123l)

    // when: the software is written

    // then: we should make it to the end
    serialize(record)
  }

  it should "be usable concurrently" in {
    // given: a bunch of `TestRecord`s
    val records = for {
      s <- 'a'.to('z').map(_.toString)
      n <- 0l.to(9l)
    } yield TestRecord(s, n)

    // when: we serialize them concurrently
    val strictResult = records.map(serialize[TestRecord])
    val concurrentResult = records.par.map(serialize[TestRecord])

    strictResult
      .zip(concurrentResult)
      .map({ case (z1, z2) => z1 sameElements z2 })
      .reduce(_ && _) should be (true)
  }

  it should "create output readable by other Thrift clients" in {
    // given: a bunch of serialized `TestRecord`s
    val data = for {
      s <- 'a'.to('z').map(_.toString)
      n <- 0l.to(9l)
    } yield (s, n)
    val thriftRecords = data.map(p =>
      new local.test.serialization.thrift.java.TestRecord(p._1, p._2)
    )
    val scroogeRecords = data.map(p => TestRecord(p._1, p._2))
    val serializedScroogeRecords = scroogeRecords.map(serialize[TestRecord])

    // when: another Thrift client deserializes them
    val serializedScroogeRecordsHopefullyDeserializedToThriftRecords =
      serializedScroogeRecords.map(
        local.test.serialization.thrift.MinimalThriftDeserializer.deserialize
      )

    // then
    serializedScroogeRecordsHopefullyDeserializedToThriftRecords should be(
      thriftRecords
    )
  }

  behavior of "ThriftDeserializer"

  it should "be able to create a `Deserializable[TestRecord]`" in {
    // given: a serialized `TestRecord`
    val bytes = serialize(TestRecord("foó", 123l))

    // when: we provide a `ThriftDeserializer[TestRecord]`
    implicit object DeserializableTestRecord
      extends ThriftDeserializer[TestRecord](TestRecord)

    // then: we should be able to deserialize `TestRecord`s
    deserialize[TestRecord](bytes)
  }

  it should "be reversible" in {
    // given: a `TestRecord` and a `ThriftDeserializer[TestRecord]`
    val record = TestRecord("foó", 123l)
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

  it should "be usable concurrently" in {
    // given: a bunch of serialized `TestRecord`s
    val records = for {
      s <- 'a'.to('z').map(_.toString)
      n <- 0l.to(9l)
    } yield TestRecord(s, n)
    val serializedRecords = records.map(serialize[TestRecord])

    // when: we deserialize them concurrently
    implicit object DeserializableTestRecord
      extends ThriftDeserializer[TestRecord](TestRecord)

    val strictResult = serializedRecords.map(deserialize[TestRecord])
    val concurrentResult = serializedRecords.par.map(deserialize[TestRecord])

    // then: the results should be the same
    strictResult
      .zip(concurrentResult)
      .map({ case (r1, r2) => r1 == r2 })
      .reduce(_ && _) should be(true)
  }

  it should "read input created by other Thrift clients" in {
    // given: a bunch of `TestRecord`s serialized by someone else
    val data = for {
      s <- 'a'.to('z').map(_.toString)
      n <- 0l.to(9l)
    } yield (s, n)
    val scroogeRecords = data.map(p => TestRecord(p._1, p._2))
    val thriftRecords = data.map(p =>
      new local.test.serialization.thrift.java.TestRecord(p._1, p._2)
    )
    val serializedThriftRecords = thriftRecords.map(
      local.test.serialization.thrift.MinimalThriftSerializer.serialize
    )

    // when: we deserialize them
    implicit object DeserializableTestRecord
      extends ThriftDeserializer[TestRecord](TestRecord)
    val serializedThriftRecordsHopefullyDeserializedToScroogeRecords =
      serializedThriftRecords.map(deserialize[TestRecord])

    // then
    serializedThriftRecordsHopefullyDeserializedToScroogeRecords should be(
      scroogeRecords.map(r => Some(r))
    )
  }

  it should "be declarable explicitly rather than implicitly" in {
    // given: some serialized records and an implicit deserializer
    val records = for {
      s <- 'a'.to('z').map(_.toString)
      n <- 0l.to(9l)
    } yield new local.test.serialization.thrift.java.TestRecord(s, n)
    val serializedRecords = records.map(
      local.test.serialization.thrift.MinimalThriftSerializer.serialize
    )
    implicit object ImplicitDeserializer
      extends ThriftDeserializer[TestRecord](TestRecord)

    // when: we create a deserializer
    val deserializer: Array[Byte] => Option[TestRecord] = {
      val foo = new ThriftDeserializer[TestRecord](TestRecord)
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
}
