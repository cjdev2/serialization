package com.cj.serialization.avro

import org.apache.avro.Schema
import org.apache.avro.specific.SpecificRecord

object Java {
  def serialize(record: SpecificRecord) : Array[Byte] = SerializableSpecificRecord.serialize(record)
  def deserialize[T >: Null <: SpecificRecord](bytes: Array[Byte], schema: Schema) : Option[T] = new AvroDeserializable[T](schema).deserialize(bytes)
}
