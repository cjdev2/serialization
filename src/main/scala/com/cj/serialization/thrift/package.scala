package com.cj.serialization

import com.twitter.scrooge.{HasThriftStructCodec3, ThriftStruct, ThriftStructCodec3}
import java.io.ByteArrayInputStream
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.transport.{TIOStreamTransport, TMemoryBuffer}

package object thrift {

  class ThriftSerializer[T <: ThriftStruct with HasThriftStructCodec3[T]]
    extends Serializable[T] {

    private val protocolFactory = new TBinaryProtocol.Factory

    def serialize(t: T): Array[Byte] = {
      val output = new TMemoryBuffer(32)
      t._codec.encode(t, protocolFactory.getProtocol(output))
      output.getArray
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
