package com.cj.serialization

import com.twitter.scrooge.ThriftStruct

package object thrift {

  class ThriftSerializer[T <: ThriftStruct] extends Serializable[T] {

    def serialize(t: T): Array[Byte] = ???
  }

  class ThriftDeserializer[T <: ThriftStruct] extends Deserializable[T] {

    def deserialize(bytes: Array[Byte]): Option[T] = ???
  }
}
