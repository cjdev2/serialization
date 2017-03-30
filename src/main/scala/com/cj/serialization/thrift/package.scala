package com.cj.serialization

import com.twitter.scrooge.{ThriftStruct, ThriftStructCodec}
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.transport.TMemoryBuffer

package object thrift {

  /**
    * This instance of [[Serializable]]`[ThriftStruct]` is sufficient
    * for serializing any child of `ThriftStruct`, i.e. serialization
    * of Scrooge-generated classes should Just Work™.
    *
    * Usage:
    * {{{
    *   import com.cj.serialization.serialize
    *   import com.cj.serialization.thrift.SerializableThriftStruct
    *   import your.awesome.thrift.struct.Awesome
    *
    *   val awesome: Awesome = ...
    *   val bytes: Array[Byte] = serialize(awesome)
    * }}}
    */
  implicit object SerializableThriftStruct extends Serializable[ThriftStruct] {

    def serialize(t: ThriftStruct): Array[Byte] = {
      val output = new TMemoryBuffer(32)
      t.write(new TBinaryProtocol(output))
      output.getArray
    }
  }

  /**
    * Create a [[Deserializable]]`[T]` instance for a scrooge-generated class `T`.
    *
    * Usage:
    * {{{
    *   import com.cj.serialization.deserialize
    *   import com.cj.serialization.thrift.ThriftDeserializer
    *   import your.awesome.thriftStruct.Awesome
    *
    *   implicit object DeserializableAwesome
    *     extends ThriftDeserializer[Awesome](Awesome)
    *
    *   val bytes: Array[Byte] = ...
    *   val optAwesome: Option[Awesome] = deserialize(bytes)
    * }}}
    *
    * @param codec The scrooge-generated codec for `T` (usually just `T`)
    * @tparam T The scrooge-generated class you'd like to be able to deserialize
    */
  class ThriftDeserializer[T <: ThriftStruct](codec : ThriftStructCodec[T])
    extends Deserializable[T] {

    def deserialize(bytes: Array[Byte]): Option[T] = {
      scala.util.Try({
        codec.decode({
          val output = new TMemoryBuffer(32)
          output.write(bytes)
          new TBinaryProtocol(output)
        })
      }).toOption.flatMap(Option.apply)
    }
  }
}
