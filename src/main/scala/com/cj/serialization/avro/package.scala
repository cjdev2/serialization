package com.cj.serialization

import org.apache.avro.Schema
import org.apache.avro.io.{DecoderFactory, EncoderFactory}
import org.apache.avro.specific.{
  SpecificDatumReader,
  SpecificDatumWriter,
  SpecificRecord
}

package object avro {

  /**
    * Turn an object that is a member of `SpecificRecord` into Avro-compliant
    * bytes.
    *
    * @param record An object that is a member of `SpecificRecord`
    * @return Avro-compliant byte-array representation
    */
  def serializeAvro(record: SpecificRecord): Array[Byte] =
    SerializableSpecificRecord.serialize(record)

  /**
    * This instance of [[Serializable]]`[SpecificRecord]`
    * is sufficient to serialize any child class of
    * `SpecificRecord`. Simply import this package
    * and the parent package into your source and call
    * [[serialize]] on your avro objects.
    */
  implicit object SerializableSpecificRecord
    extends Serializable[SpecificRecord] {

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
    * probably fine. If you're worried about performance, use `AvroDeserializer`
    *
    * @param bytes  Bytes that you'd like to try to parse.
    * @param schema The schema against which you'd like to parse your bytes.
    * @tparam T The Avro-generated type that the schema represents.
    * @return An `Optional` of your Avro-generated type.
    */
  def deserializeAvro[T >: Null <: SpecificRecord](
                                                    schema: Schema,
                                                    bytes: Array[Byte]
                                                  ): Option[T] =
    new AvroDeserializable[T](schema).deserialize(bytes)

  /**
    * We need a separate instance of
    * [[Deserializable]]`[Bar]` for each child `Bar` of
    * `SpecificRecord`, and we need to explicitly
    * supply the `Schema` of `Bar` as a constructor argument.
    *
    * In your source, invoke as
    * {{{
    *   implicit object Foo extends AvroDeserializable[Bar](Bar.getClassSchema)
    * }}}
    * to put a `Deserializable[Bar]` into scope, then just call [[deserialize]]
    * on your bytes thereafter.
    *
    * @param schema The `Schema` of `T`, typically `T.getClassSchema`
    * @tparam T A child class of `SpecificRecord`
    */
  class AvroDeserializable[T >: Null <: SpecificRecord](schema: Schema)
    extends Deserializable[T] {

    private val reader = new SpecificDatumReader[T](schema)
    private val factory = DecoderFactory.get

    def deserialize(bytes: Array[Byte]): Option[T] = {
      val decoder = factory.binaryDecoder(bytes, null)

      scala.util.Try(reader.read(null, decoder))
        .toOption.flatMap(Option.apply)
    }
  }
}
