import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class ThriftTest extends FlatSpec with Matchers with PropertyChecks {

  import com.cj.serialization._
  import com.cj.serialization.thrift._
  import local.test.serialization.thrift.scala.TestRecord

  "ThriftDemo" should "not be out of date" in {
    ThriftDemo.main(args = Array[String]())
  }

  behavior of "ThriftSerializer"

  it should "be accessible for TestRecord, never throw, never return null" in {
    forAll { (s: String, n: Long) =>
      // given: a `TestRecord`
      val record = TestRecord(s, n)

      // when: we serialize
      val result = Try(serialize(record))

      // then: it should not have thrown or returned a null
      result.isFailure should be(false)
      result.get == null should be(false)
    }
  }

  it should "be usable concurrently" in {
    forAll { (pairs: List[(String, Long)]) =>
      // given: a bunch of `TestRecord`s
      val records = pairs.map(pair => TestRecord(pair._1, pair._2))

      // when: we serialize them concurrently
      val strictResult = records.map(serialize[TestRecord])
      val concurrentResult = records.par.map(serialize[TestRecord])

      // then: the strict result is the same as the concurent result
      strictResult.zip(concurrentResult)
        .forall(pair => pair._1 sameElements pair._2) should be(true)
    }
  }

  it should "create output readable by other Thrift clients" in {
    forAll { (s: String, n: Long) =>
      // given: a serialized `TestRecord`
      val thriftRecord =
        new local.test.serialization.thrift.java.TestRecord(s, n)
      val scroogeRecord = TestRecord(s, n)
      val serializedScroogeRecord = serialize(scroogeRecord)

      // when: another Thrift client deserializes it
      val perhapsThriftRecord =
        local.test.serialization.thrift.MinimalThriftDeserializer
          .deserialize(serializedScroogeRecord)

      // then: the deserialize record should match
      perhapsThriftRecord should be(thriftRecord)
    }
  }

  behavior of "ThriftDeserializer"

  it should "be able to deserialize `TestRecord`s" in {
    forAll { (s: String, n: Int) =>
      // given: a serialized `TestRecord`
      val record = TestRecord(s, n)
      val bytes = serialize(record)

      // when: we create a `ThriftDeserializer[TestRecord]`
      implicit object DeserializableTestRecord
        extends ThriftDeserializer[TestRecord](TestRecord)

      // then: we should be able to deserialize `TestRecord`s
      deserialize[TestRecord](bytes).contains(record)
    }
  }

  it should "never throw an exception or return a null" in {
    forAll { (bs: Array[Byte]) =>
      // given: A `ThriftDeserializer[TestRecord]`
      implicit object DeserializableTestRecord
        extends ThriftDeserializer[TestRecord](TestRecord)

      // when: we deserialize
      val result = Try(deserialize[TestRecord](bs))

      // then: we should not have thrown or returned a null
      result.isFailure should be(false)
      result.get == null should be(false)
      // I don't really trust this test, because the byte arrays are almost
      // certainly too far off from being Thrift byte arrays that the
      // deserializer rejects them out of hand. I'd like to find a way to
      // create some byte arrays that are _close_ to being Thrift arrays but
      // are a bit corrupt or something like that.
    }
  }

//  it should "be reversible" in {
//    // given: a `TestRecord` and a `ThriftDeserializer[TestRecord]`
//    val record = TestRecord("foó", 123l)
//    implicit object DeserializableTestRecord
//      extends ThriftDeserializer[TestRecord](TestRecord)
//
//    // when: we deserialize a serialized record
//    val result: Option[TestRecord] = deserialize(serialize(record))
//
//    // then: the record should survive the round trip
//    result should be(Some(record))
//  }
//
//  it should "return `None` when given bad input" in {
//    // given: some bad bytes
//    val badBytes = "baaad".toCharArray.map(_.toByte)
//    implicit object DeserializableTestRecord
//      extends ThriftDeserializer[TestRecord](TestRecord)
//
//    // when: we serialize them
//    val result: Option[TestRecord] = deserialize(badBytes)
//
//    // then: the result should be `None`
//    result should be(None)
//  }
//
//  it should "be usable concurrently" in {
//    // given: a bunch of serialized `TestRecord`s
//    val records = for {
//      s <- 'a'.to('z').map(_.toString)
//      n <- 0l.to(9l)
//    } yield TestRecord(s, n)
//    val serializedRecords = records.map(serialize[TestRecord])
//
//    // when: we deserialize them concurrently
//    implicit object DeserializableTestRecord
//      extends ThriftDeserializer[TestRecord](TestRecord)
//
//    val strictResult = serializedRecords.map(deserialize[TestRecord])
//    val concurrentResult = serializedRecords.par.map(deserialize[TestRecord])
//
//    // then: the results should be the same
//    strictResult
//      .zip(concurrentResult)
//      .map({ case (r1, r2) => r1 == r2 })
//      .reduce(_ && _) should be(true)
//  }
//
//  it should "read input created by other Thrift clients" in {
//    // given: a bunch of `TestRecord`s serialized by someone else
//    val data = for {
//      s <- 'a'.to('z').map(_.toString)
//      n <- 0l.to(9l)
//    } yield (s, n)
//    val scroogeRecords = data.map(p => TestRecord(p._1, p._2))
//    val thriftRecords = data.map(p =>
//      new local.test.serialization.thrift.java.TestRecord(p._1, p._2)
//    )
//    val serializedThriftRecords = thriftRecords.map(
//      local.test.serialization.thrift.MinimalThriftSerializer.serialize
//    )
//
//    // when: we deserialize them
//    implicit object DeserializableTestRecord
//      extends ThriftDeserializer[TestRecord](TestRecord)
//    val serializedThriftRecordsHopefullyDeserializedToScroogeRecords =
//      serializedThriftRecords.map(deserialize[TestRecord])
//
//    // then
//    serializedThriftRecordsHopefullyDeserializedToScroogeRecords should be(
//      scroogeRecords.map(r => Some(r))
//    )
//  }
//
//  it should "be declarable explicitly rather than implicitly" in {
//    // given: some serialized records and an implicit deserializer
//    val records = for {
//      s <- 'a'.to('z').map(_.toString)
//      n <- 0l.to(9l)
//    } yield new local.test.serialization.thrift.java.TestRecord(s, n)
//    val serializedRecords = records.map(
//      local.test.serialization.thrift.MinimalThriftSerializer.serialize
//    )
//    implicit object ImplicitDeserializer
//      extends ThriftDeserializer[TestRecord](TestRecord)
//
//    // when: we create a deserializer
//    val deserializer: Array[Byte] => Option[TestRecord] = {
//      val foo = new ThriftDeserializer[TestRecord](TestRecord)
//      foo.deserialize
//    }
//
//    val implicitResult = serializedRecords.map(deserialize[TestRecord])
//    val explicitResult = serializedRecords.par.map(deserializer)
//
//    // then: our serializer should do all the things
//    explicitResult
//      .zip(implicitResult)
//      .map({ case (r1, r2) => r1 == r2 })
//      .reduce(_ && _) should be(true)
//  }
}
