package com.cj.serialization

import org.apache.avro.Schema
import org.apache.avro.io.{DecoderFactory, EncoderFactory}
import org.apache.avro.specific.{SpecificDatumReader, SpecificDatumWriter, SpecificRecord}

package object avro {

  /**
    * Turn an object that is a member of `SpecificRecord` into Avro-compliant
    * bytes.
    *
    * @param record An object that is a member of `SpecificRecord`
    * @return Avro-compliant byte-array representation
    */
  def serializeAvro(record: SpecificRecord): Array[Byte] =
    SerializeSpecificRecord.serialize(record)

  /**
    * This instance of [[Serialize]]`[SpecificRecord]` is sufficient
    * for serializing any child of `SpecificRecord`, i.e. serialization
    * of Avro-generated classes should Just Work™.
    *
    * Usage:
    * {{{
    *   import com.cj.serialization.serialize
    *   import com.cj.serialization.avro.SerializeSpecificRecord
    *   import your.awesome.avro.record.Awesome
    *
    *   val awesome: Awesome = ...
    *   val bytes: Array[Byte] = serialize(awesome)
    * }}}
    */
  implicit object SerializeSpecificRecord
    extends Serialize[SpecificRecord] {

    private val factory = EncoderFactory.get

    def serialize(t: SpecificRecord): Array[Byte] = {
      val output = new java.io.ByteArrayOutputStream
      val encoder = factory.binaryEncoder(output, null)

      new SpecificDatumWriter[SpecificRecord](t.getSchema).write(t, encoder)
      encoder.flush()
      output.close()
      output.toByteArray
    }
  }

  /**
    * Attempt to deserialize the provided `bytes` using the provided `schema`.
    * Creates a new instance of `AvroDeserializable` on each call, which is
    * probably fine. If you're worried about performance, use `AvroDeserializable`
    *
    * @param bytes  Bytes that you'd like to try to parse.
    * @param schema The schema against which you'd like to parse your bytes.
    * @tparam T The Avro-generated type that the schema represents.
    * @return An `Optional` of your Avro-generated type.
    */
  def deserializeAvro[T >: Null <: SpecificRecord](
                                                    schema: Schema,
                                                    bytes: Array[Byte]
                                                  ): Result[T] =
    new DeserializeSpecificRecord[T](schema).deserialize(bytes)

  /**
    * Create a [[Deserialize]]`[T]` instance for an avro-generated class `T`.
    *
    * Usage:
    * {{{
    *   import com.cj.serialization.deserialize
    *   import com.cj.serialization.avro.DeserializeSpecificRecord
    *   import your.awesome.avro.record.Awesome
    *
    *   implicit object DeserializeAwesome
    *     extends DeserializeSpecificRecord[Awesome](Awesome.getClassSchema)
    *
    *   val bytes: Array[Byte] = ...
    *   val optAwesome: Option[Awesome] = deserialize(bytes)
    * }}}
    *
    * @param schema The avro-generated schema for `T` (usually `T.getClassSchema`)
    * @tparam T The avro-generated class you'd like to be able to deserialize
    */
  class DeserializeSpecificRecord[T >: Null <: SpecificRecord](schema: Schema)
    extends Deserialize[T] {

    private val reader = new SpecificDatumReader[T](schema)
    private val factory = DecoderFactory.get

    def deserialize(bytes: Array[Byte]): Result[T] = {
      val decoder = factory.binaryDecoder(bytes, null)

      Result.safely(reader.read(null, decoder))
    }
  }
}
