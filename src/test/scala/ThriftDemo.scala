object ThriftDemo extends App {

  // TODO: Write the demo!

  import com.cj.serialization._
  import com.cj.serialization.thrift._

  // thrift-generated classes
  import local.test.serialization.thrift.scala.TestRecord

  implicit object DeserializableTestRecord
    extends ThriftDeserializer[TestRecord](TestRecord)

  val record: TestRecord = TestRecord("foó", 123l)

  assert({
    println(record)
    println(serialize(record).mkString(","))
    println(deserialize[TestRecord](serialize(record)))
    deserialize[TestRecord](serialize(record)).get == record
  })
}
