package com.cj.serialization

import com.twitter.scrooge.ThriftStruct

package object thrift {

  implicit object SerializableThriftStruct extends Serializable[ThriftStruct] {
    def serialize(t: ThriftStruct): Array[Byte] = {
      implicitly[Serializable[String]].serialize(t.toString)
    }
  }

  class ThriftDeserializable[T <: ThriftStruct] extends Deserializable[T] {
    def deserialize(bytes: Array[Byte]): Option[T] = None
  }

  def makeThriftDeserializer[T <: ThriftStruct]: Array[Byte] => Option[T] = {

    implicit object DeserializableStruct extends ThriftDeserializable[T]

    bytes => deserialize(bytes)
  }
}
