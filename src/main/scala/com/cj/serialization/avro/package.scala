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
    * This instance of [[Serializable]]`[SpecificRecord]`
    * is sufficient to serialize any child class of
    * [[SpecificRecord]]. Simply import this package
    * and the parent package into your source and call
    * [[serialize]] on your avro objects.
    */
  implicit object SerializableSpecificRecord
    extends Serializable[SpecificRecord] {

    def serialize(t: SpecificRecord): Array[Byte] = {
      val output = new java.io.ByteArrayOutputStream
      val writer = new SpecificDatumWriter[SpecificRecord]
      val encoder = EncoderFactory.get.binaryEncoder(output, null)

      writer.setSchema(t.getSchema)
      writer.write(t, encoder)
      encoder.flush()
      output.close()
      output.toByteArray
    }
  }

  /**
    * We need a separate instance of
    * [[Deserializable]]`[Bar]` for each child `Bar` of
    * [[SpecificRecord]], and we need to explicitly
    * supply the [[Schema]] of `Bar` as a constructor argument.
    *
    * In your source, invoke as
    * {{{
    *   implicit object Foo extends AvroDeserializable[Bar](Bar.getClassSchema)
    * }}}
    * to put a `Deserializable[Bar]` into scope, then just call [[deserialize]]
    * on your bytes thereafter.
    *
    * @param schema The [[Schema]] of `T`, typically `T.getClassSchema`
    * @tparam T A child class of [[SpecificRecord]]
    */
  class AvroDeserializable[T >: Null <: SpecificRecord](schema: Schema)
    extends Deserializable[T] {

    private val reader =
      new SpecificDatumReader[T](schema)

    private var decoder =
      DecoderFactory.get.binaryDecoder(Array[Byte](), null)

    def deserialize(bytes: Array[Byte]): Option[T] = {
      decoder = DecoderFactory.get.binaryDecoder(bytes, decoder)
      scala.util.Try(reader.read(null, decoder)).toOption.flatMap({
        case null => None
        case t => Some(t)
      })
    }
  }

  /**
    * An alternative interface for deserializing Avro objects.
    *
    * In your source, invoke as
    * {{{
    *   val foo = makeAvroDeserializer[Bar](Bar.getClassSchema)
    * }}}
    * the call `foo` on your bytes.
    *
    * @param schema The [[Schema]] of `T`, typically `T.getClassSchema`
    * @tparam T A child class of [[SpecificRecord]]
    * @return A function that contains in its closure a single, reusable
    *         instance of [[AvroDeserializable]]`[T]` for minimal overhead
    */
  def makeAvroDeserializer[T >: Null <: SpecificRecord](schema: Schema)
  : Array[Byte] => Option[T] = {

    implicit object DeserializableRecord extends AvroDeserializable[T](schema)

    bytes => deserialize(bytes)
  }

  /* Legacy API */

  @deprecated("`serialize` from `com.cj.serialization` makes this type unnecessary.")
  type RecordSerializer[-T] = T => Array[Byte]

  @deprecated("`deserialize` from `com.cj.serialization` makes this type unnecessary.")
  type RecordDeserializer[+T] = Array[Byte] => T

  /**
    * Make a [[RecordSerializer]] for class `T`, passing through intermediate
    * avro-generated class `U`.
    *
    * @param f A function that converts your type `T` to avro-type `U`
    * @tparam T Your starting type
    * @tparam U Avro-generated type
    * @return A [[RecordSerializer]] that consumes `T`s
    */
  @deprecated("`serialize` from `com.cj.serialization` makes this method unnecessary.")
  def mkSerializer[T, U <: SpecificRecord](f: (T => U)): RecordSerializer[T] = {
    val avroSerializer = mkAvroSerializer[U]()
    record => avroSerializer(f(record))
  }

  /**
    * Make a [[RecordDeserializer]] of `Option[T]`, passing through intermediate
    * avro-generated class `U`.
    *
    * @param f A function that converts avro-type `U` to your type `T`
    * @param schema The `Schema` of `U`, typically `U.getClassSchema`
    * @tparam T Your ending type
    * @tparam U Avro-generated type
    * @return A `RecordDeserializer` that produces `Option[T]`s
    */
  @deprecated("`makeAvroDeserializer` replaces this method.")
  def mkDeserializer[T, U >: Null <: SpecificRecord](f: U => T, schema: Schema)
  : RecordDeserializer[Option[T]] = {
    val avroDeserializer = mkAvroDeserializer(schema)
    bytes => {
      val avroRec: U = avroDeserializer(bytes)
      if (avroRec == null) None
      else Some(f(avroRec))
    }
  }

  /**
    * Make a [[RecordSerializer]] for avro-generated class `T`.
    *
    * @tparam T An Avro-generated class extending [[SpecificRecord]]
    * @return A [[RecordSerializer]] that consumes `T`s
    */
  @deprecated("`serialize` from `com.cj.serialization` makes this method unnecessary.")
  def mkAvroSerializer[T <: SpecificRecord](): RecordSerializer[T] = {
    SerializableSpecificRecord.serialize
  }

  /**
    * Make a [[RecordDeserializer]] for avro-generated class `T`.
    *
    * @param schema The `Schema` of `U`, typically `U.getClassSchema`
    * @tparam T An Avro-generated class extending `SpecificRecord`
    * @return A `RecordDeserializer` that produces `T`s
    */
  @deprecated("`makeAvroDeserializer` replaces this method.")
  def mkAvroDeserializer[T >: Null <: SpecificRecord](schema: Schema)
  : RecordDeserializer[T] = {

    implicit object DeserializableRecord extends AvroDeserializable[T](schema)

    bytes => deserialize(bytes).getOrElse({
      val bs = bytes.map(_.toChar).mkString
      val cls = schema.getFullName
      val msg = s"mkAvroDeserializer: Failed to parse $bs as Avro class $cls"
      throw new RuntimeException(msg)
    })
  }
}
