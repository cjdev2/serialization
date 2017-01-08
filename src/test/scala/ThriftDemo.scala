import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.transport.TIOStreamTransport

object ThriftDemo extends App {

  // thrift-generated class
  import local.test.serialization.thrift.scala.TestRecord

  def serialize(t: TestRecord): Array[Byte] = {
    val output = new ByteArrayOutputStream
    t._codec.encode(
      t, new TBinaryProtocol.Factory().getProtocol(
        new TIOStreamTransport(output)
      )
    )
    output.toByteArray
  }

  def deserialize(bytes: Array[Byte]): Option[TestRecord] = {
    scala.util.Try(
      TestRecord.decode(
        new TBinaryProtocol.Factory().getProtocol(
          new TIOStreamTransport(new ByteArrayInputStream(bytes))
        )
      )
    ).toOption.flatMap(Option.apply)
  }

  val record: TestRecord = TestRecord("foó", 123l)

  assert({
    println(record)
    println(serialize(record).mkString(","))
    println(deserialize(serialize(record)))
    deserialize(serialize(record)).get == record
  })
}
