object AvroDemo extends App {

  import com.cj.serialization._
  import com.cj.serialization.avro._

  // Avro-generate class
  import local.test.serialization.avro.TestRecord

  val record: TestRecord = new TestRecord("foó", 123l)
  println(record)

  // we can serialize anything that extends `SpecificRecord`
  // for free, including our `TestRecord` class
  val serializedByUs: Array[Byte] = serialize(record)
  println(java.util.Base64.getEncoder.encodeToString(serializedByUs))

  // Avro knows how to clean up its own mess, it just
  // needs a tiny bit of help from you, the programmer
  implicit object DeserializeSpecificRecordTestRecord
    extends DeserializeSpecificRecord[TestRecord](TestRecord.getClassSchema)

  // now, `deserialize[TestRecord]: Array[Byte] => Option[TestRecord]`
  // is in scope and works as expected

  // Avro records survive the round trip
  assert(
    deserialize[TestRecord](serializedByUs).getOrThrow == record
  )

  // Avro clients agnostic of this library can
  // deserialize records what we serialized
  val deserializedByThem: TestRecord =
    local.test.serialization.avro
      .MinimalAvroDeserializer
      .deserialize(serializedByUs)
  println(deserializedByThem)

  assert(
    deserializedByThem == record
  )

  // and we can deserialize records serialized
  // by Avro clients agnostic of this library
  val newRecord: TestRecord = new TestRecord("bår", 456l)
  println(newRecord)

  val serializedByThem: Array[Byte] =
    local.test.serialization.avro
      .MinimalAvroSerializer
      .serialize(newRecord)
  println(java.util.Base64.getEncoder.encodeToString(serializedByThem))

  val deserializedByUs: Option[TestRecord] =
    deserialize(serializedByThem).success
  println(deserializedByUs)

  assert(
    deserializedByUs.get == newRecord
  )
}
