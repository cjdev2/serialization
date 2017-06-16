package com.cj.serialization
package avro

import java.util.Optional

import com.cj.serialization.Java._
import org.apache.avro.Schema
import org.apache.avro.specific.SpecificRecord

/**
  * Java API for `com.cj.serialization.avro`.
  */
object Java {

  /**
    * Turn an object that is a member of `SpecificRecord` into Avro-compliant
    * bytes.
    *
    * @param record An object that is a member of `SpecificRecord`
    * @return Avro-compliant byte-array representation
    */
  def serializeAvro(record: SpecificRecord) : Array[Byte] =
    SerializeSpecificRecord.serialize(record)

  /**
    * A class to represent a serializer for the given class `T` extending
    * `SpecificRecord`. Strictly speaking, this class is unnecessary, as
    * `serialize` works generically for any class extending `SpecificRecord`;
    * however, this class is included in case a class representation is
    * necessary to satisfy some API.
    */
  class AvroSerializeJ[T <: SpecificRecord] extends SerializeJ[T] {
    def serialize(t: T): Array[Byte] =
      SerializeSpecificRecord.serialize(t)
  }

  /**
    * Attempt to deserialize the provided `bytes` using the provided `schema`.
    * Creates a new instance of `AvroDeserializable` on each call, which is
    * probably fine. If you're worried about it, use `AvroDeserializerJ`
    *
    * @param bytes  Bytes that you'd like to try to parse.
    * @param schema The schema against which you'd like to parse your bytes.
    * @tparam T The Avro-generated type that the schema represents.
    * @return An `Optional` of your Avro-generated type.
    */
  def deserializeAvro[T >: Null <: SpecificRecord](
                                                    schema: Schema,
                                                    bytes: Array[Byte]
                                                  ): Optional[T] =
    new DeserializeSpecificRecord[T](schema)
      .deserialize(bytes).fold(Optional.empty[T])(t => Optional.of(t))

  /**
    * A class to represent a deserializer for the given class `T` extending
    * `SpecificRecord`. Strictly speaking, this class is unnecessary, as
    * `deserialize` works generically for any class extending `SpecificRecord`;
    * however, this class is included in case a class representation is
    * necessary to satisfy some API.
    *
    * @param schema A schema representing the class `T`.
    * @tparam T The class you are expecting your bytes to conform to.
    */
  class AvroDeserializeJ[T >: Null <: SpecificRecord](schema: Schema)
    extends DeserializeJ[T] {

    private object DeserializeSpecificRecord extends DeserializeSpecificRecord[T](schema)

    def deserialize(bytes: Array[Byte]): Optional[T] =
      DeserializeSpecificRecord.deserialize(bytes)
        .fold(Optional.empty[T])(v => Optional.of(v))
  }
}
