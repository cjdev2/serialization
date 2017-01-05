package com.cj.serialization

package object thrift {

  class ThriftSerializer[T] extends Serializable[T] {

    def serialize(t: T): Array[Byte] = ???
  }

  class ThriftDeserializer[T] extends Deserializable[T] {

    def deserialize(bytes: Array[Byte]): Option[T] = ???
  }
}
