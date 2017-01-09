object ThriftDemo extends App {

  // TODO: Write the demo!

  import com.cj.serialization._
  import com.cj.serialization.thrift._

  // thrift-generated class
  import local.test.serialization.thrift.scala.TestRecord

  val record: TestRecord = TestRecord("foó", 123l)

  // Scrooge-generated classes are instances of `Serializable` by default
  serialize(record)

  // To deserialize, create a
}
