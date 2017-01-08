object ThriftDemo extends App {

  // TODO: Write the demo!

  import com.cj.serialization._
  import com.cj.serialization.thrift._

  // thrift-generated class
  import local.test.serialization.thrift.scala.TestRecord

  implicit object DeserializableTestRecord
    extends ThriftDeserializer[TestRecord](TestRecord)

  val record: TestRecord = TestRecord("foó", 123l)

  assert({
    deserialize[TestRecord](serialize(record)).contains(record)
  })
}
