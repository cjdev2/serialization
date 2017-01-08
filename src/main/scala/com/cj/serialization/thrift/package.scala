package com.cj.serialization

import com.twitter.scrooge.{HasThriftStructCodec3, ThriftStruct, ThriftStructCodec3}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.transport.TIOStreamTransport

package object thrift {

  class ThriftSerializer[T <: ThriftStruct with HasThriftStructCodec3[T]]
    extends Serializable[T] {

    private val protocolFactory = new TBinaryProtocol.Factory

    def serialize(t: T): Array[Byte] = {
      val baos = new ByteArrayOutputStream()
      t._codec.encode(t, protocolFactory.getProtocol(new TIOStreamTransport(baos)))
      baos.toByteArray
    }
  }

  class ThriftDeserializer[T <: ThriftStruct with HasThriftStructCodec3[T]](
    codec : ThriftStructCodec3[T]
  ) extends Deserializable[T] {

    private val protocolFactory = new TBinaryProtocol.Factory

    def deserialize(bytes: Array[Byte]): Option[T] = {
      scala.util.Try({
        codec.decode(
          protocolFactory.getProtocol(
            new TIOStreamTransport(
              new ByteArrayInputStream(bytes))
          )
        )
      }).toOption.flatMap(Option.apply)
    }
  }
}
