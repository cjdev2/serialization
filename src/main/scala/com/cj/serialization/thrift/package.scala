package com.cj.serialization

import com.twitter.scrooge.ThriftStruct

package object thrift {

//  implicit object SerializableThriftStruct extends Serializable[ThriftStruct] {
//    def serialize(t: ThriftStruct): Array[Byte] = {
//      ???
//      Array()
//    }
//  }
//
//  class ThriftDeserializable[T <: ThriftStruct] extends Deserializable[T] {
//    def deserialize(bytes: Array[Byte]): Option[T] = {
//      ???
//      None
//    }
//  }

  class ThriftSerializer[T](stuffs: String) extends Serializable[T] {

    def serialize(t: T): Array[Byte] = ???

  }

//  def makeThriftDeserializer[T <: ThriftStruct]: Array[Byte] => Option[T] = {
//
//    implicit object DeserializableStruct extends ThriftDeserializable[T]
//
//    bytes => deserialize(bytes)
//  }
}
