import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import com.twitter.scrooge.ThriftStructCodec
import org.apache.thrift.protocol.{TBinaryProtocol, TProtocolFactory}
import org.apache.thrift.transport.TIOStreamTransport

object ThriftDemo extends App {

  import local.test.serialization.thrift.scala.TestRecord // Thrift-generated class

  val protocolFactory: TProtocolFactory = new TBinaryProtocol.Factory()

  def serialize(t: TestRecord): Array[Byte] = {
    val output = new ByteArrayOutputStream
    t._codec.encode(t, protocolFactory.getProtocol(new TIOStreamTransport(output)))
    output.toByteArray
  }

  def fromBytes(codec: ThriftStructCodec[TestRecord], bytes: Array[Byte]): TestRecord = {
    codec.decode(protocolFactory.getProtocol(
      new TIOStreamTransport(new ByteArrayInputStream(bytes))
    ))
  }

  def deserialize(bytes: Array[Byte]): Option[TestRecord] = {
    scala.util.Try(fromBytes(TestRecord("",0)._codec, bytes)).toOption
  }

  val record: TestRecord = TestRecord("foó", 123l)

  println(deserialize(serialize(record)))
}
