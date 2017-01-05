import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}

import com.twitter.scrooge.ThriftStructCodec
import org.apache.thrift.protocol.{TBinaryProtocol, TProtocolFactory}
import org.apache.thrift.transport.TIOStreamTransport

object ThriftDemo extends App {

  import local.test.serialization.thrift.scala.TestRecord // Thrift-generated class

  def codec: ThriftStructCodec[TestRecord] = record._codec
  def protocolFactory: TProtocolFactory = new TBinaryProtocol.Factory()

  def toBytes(obj: TestRecord): Array[Byte] = {
    val buf = new ByteArrayOutputStream
    val proto = protocolFactory.getProtocol(new TIOStreamTransport(buf))
    codec.encode(obj, proto)
    buf.toByteArray
  }

  def fromBytes(bytes: Array[Byte]): TestRecord = fromInputStream(new ByteArrayInputStream(bytes))

  def fromInputStream(stream: InputStream): TestRecord = {
    val proto = protocolFactory.getProtocol(new TIOStreamTransport(stream))
    codec.decode(proto)
  }

  val record: TestRecord = TestRecord("foó", 123l)

  println(fromBytes(toBytes(record)))

//  assert({
//    toBytes(record)
//    true
//  })
}
