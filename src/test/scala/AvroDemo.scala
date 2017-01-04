object AvroDemo extends App {

  import com.cj.serialization._
  import com.cj.serialization.avro._
  import local.test.serialization.avro.TestRecord // Avro-generate class

  val record: TestRecord = new TestRecord("foó", 123l)

  // we can serialize `TestRecord`s out of the box, because we can serialize
  // anything that extends `SpecificRecord` for free, and these records can be
  // read by Avro clients that are agnostic of this library
  val serializedByUs: Array[Byte] = serialize(record)

  // Avro knows how clean up its own mess
  // it just needs a tiny bit of help from you, the programmer
  implicit object TestRecordDeserializer
    extends AvroDeserializable[TestRecord](TestRecord.getClassSchema)

  assert(
    // Avro records survive a round trip
    deserialize[TestRecord](serializedByUs).contains(record)
  )

  // we can deserialize avro records serialized by Avro clients that are
  // agnostic of this library
  val avroRecordSerializedBySomeoneElse: Array[Byte]
    = Array(6, 98, 97, 122, -112, 7).map(_.toByte)
  val optRec = deserialize[TestRecord](avroRecordSerializedBySomeoneElse)

  assert({
    optRec.isDefined &&
      optRec.map(_.getFoo).contains("baz") &&
      optRec.map(_.getBar).contains(456l)
  })
}
