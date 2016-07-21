package com.cj.serialization.avro // why are we making the test accessible to the world?

import org.scalatest.{FlatSpec, Matchers}

import com.cj.serialization.avro.Util.{RecordDeserializer, RecordSerializer, mkAvroDeserializer, mkAvroSerializer, mkDeserializer, mkSerializer}
import com.cj.serialization.avro.autogen.TestRecord

class UtilTest extends FlatSpec with Matchers {

  // TestRecord is Avro-generated java class
  // SomeRecord is native Scala class
  case class SomeRecord(imAString : String)

  "TestRecord" should "survive serialization-deserialization" in {
    // given: a TestRecord
    val record = new TestRecord("foo")

    // when: it is serialized and then deserialized
    val serializer : RecordSerializer[TestRecord] = mkAvroSerializer[TestRecord]()
    val deserializer : RecordDeserializer[TestRecord] = mkAvroDeserializer(TestRecord.getClassSchema)
    val foo = serializer(record)
    val s = foo.toString

    val result : TestRecord = deserializer(foo)
    val result2 : TestRecord = mkAvroDeserializer(TestRecord.getClassSchema)(foo)
    val result3 : TestRecord = mkAvroDeserializer(TestRecord.getClassSchema)(foo)
    val result4 : TestRecord = mkAvroDeserializer(TestRecord.getClassSchema)(foo)

    // then: the result should be the same as the starting record
    result should be (record)
  }

  "TestRecord" should "survive serialization-deserialization when reusing (de)serializer" in {
    // given: a TestRecord
    val record1 = new TestRecord("foo")
    val record2 = new TestRecord("foo2")
    val serializer : RecordSerializer[TestRecord] = mkAvroSerializer[TestRecord]()
    val deserializer : RecordDeserializer[TestRecord] = mkAvroDeserializer(TestRecord.getClassSchema)

    // when: it is serialized and then deserialized
    val foo1 = serializer(record1)
    val result1 : TestRecord = deserializer(foo1)
    val foo2 = serializer(record2)
    val result2 : TestRecord = deserializer(foo2)

    // then: the result should be the same as the starting record
    result1 should be (record1)
    result2 should be (record2)
  }

  "SomeRecord" should "survive encoding-decoding" in {
    // given: a SomeRecord
    val record = SomeRecord("foo")
    def someRecordToAvro : SomeRecord => TestRecord = { case SomeRecord(str) => new TestRecord(str) }
    def someRecordFromAvro : TestRecord => SomeRecord = { avro => SomeRecord(avro.getFoo) }
    val encode = mkSerializer(someRecordToAvro)
    val decode = mkDeserializer(someRecordFromAvro, TestRecord.getClassSchema)


    //when: it is encoded and then decoded
    val result = decode(encode(record))

    //then: we get back what we started with
    result should be (Some(record))
  }
}
