package com.cj.serialization

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import com.twitter.scrooge.{HasThriftStructCodec3, ThriftStruct, ThriftStructCodec3}
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.transport.{TIOStreamTransport, TMemoryBuffer}

package object thrift {

  class ThriftSerializer[T <: HasThriftStructCodec3[T] with ThriftStruct] extends Serializable[T] {
    val protocolFactory = new TBinaryProtocol.Factory

    def serialize(t: T): Array[Byte] = {
      val buf = new TMemoryBuffer(32)
      val oprot = protocolFactory.getProtocol(buf)
      t._codec.encode(t, oprot)
      buf.getArray
    }
  }

  class ThriftDeserializer
  [T <: HasThriftStructCodec3[T] with ThriftStruct](codec : ThriftStructCodec3[T])
    extends Deserializable[T] {
    val protocolFactory = new TBinaryProtocol.Factory

    def deserialize(bytes: Array[Byte]): Option[T] = {
      scala.util.Try({
        val buf = new ByteArrayInputStream(bytes)
        val oprot = protocolFactory.getProtocol(new TIOStreamTransport(buf))
        codec.decode(oprot)
      }).toOption.flatMap(Option.apply)
    }
  }
}
