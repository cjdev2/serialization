package com.cj.serialization

import com.twitter.scrooge.{ThriftStruct, ThriftStructCodec3}
import org.apache.thrift.protocol.{TBinaryProtocol, TProtocol}
import org.apache.thrift.transport.{TMemoryBuffer, TTransport}

package object thrift {

  private val getProtocol: TTransport => TProtocol = {
    val factory = new TBinaryProtocol.Factory
    factory.getProtocol
  }

  implicit object SerializableThriftStruct extends Serializable[ThriftStruct] {

    def serialize(t: ThriftStruct): Array[Byte] = {
      val output = new TMemoryBuffer(32)
      t.write(getProtocol(output))
      output.getArray
    }
  }

  class ThriftDeserializer[T <: ThriftStruct](codec : ThriftStructCodec3[T])
    extends Deserializable[T] {

    def deserialize(bytes: Array[Byte]): Option[T] = {
      scala.util.Try({
        codec.decode({
          val output = new TMemoryBuffer(32)
          output.write(bytes)
          getProtocol(output)
        })
      }).toOption.flatMap(Option.apply)
    }
  }
}
