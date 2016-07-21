package com.cj.serialization.avro

import java.io.ByteArrayOutputStream

import org.apache.avro.Schema
import org.apache.avro.io.{BinaryDecoder, BinaryEncoder, DatumWriter, DecoderFactory, EncoderFactory}
import org.apache.avro.specific.{SpecificDatumReader, SpecificDatumWriter, SpecificRecord}

object Util {
  type RecordSerializer[T] = T => Array[Byte]
  type RecordDeserializer[T] = Array[Byte] => T

  def mkSerializer[T, U <: SpecificRecord](f: (T => U)): RecordSerializer[T] = {
    val serializer = mkAvroSerializer[U]()
    record => serializer(f(record))
  }

  def mkDeserializer[T, U >: Null <: SpecificRecord](f: U => T, schema: Schema): RecordDeserializer[Option[T]] = {
    val deserializer: Array[Byte] => U = mkAvroDeserializer(schema)
    bytes => {
      val avro: U = deserializer(bytes)
      if (avro == null) None
      else Some(f(avro))
    }
  }

  def mkAvroSerializer[T <: SpecificRecord]: Unit => RecordSerializer[T] = _ => {
    val output = new ByteArrayOutputStream()
    val writer: DatumWriter[T] = new SpecificDatumWriter[T]()
    var encoder: BinaryEncoder = EncoderFactory.get().binaryEncoder(output, null)
    record => {
      output.reset()
      encoder = EncoderFactory.get().binaryEncoder(output, encoder)
      writer.setSchema(record.getSchema)
      writer.write(record, encoder)
      encoder.flush()
      output.close()
      output.toByteArray
    }
  }

  def mkAvroDeserializer[T >: Null <: SpecificRecord]: Schema => RecordDeserializer[T] = schema => {
    val reader: SpecificDatumReader[T] = new SpecificDatumReader[T](schema)
    var decoder: BinaryDecoder = DecoderFactory.get().binaryDecoder(Array[Byte](), null)
    bytes => {
      decoder = DecoderFactory.get().binaryDecoder(bytes, decoder)
      reader.read(null, decoder)
    }
  }
}
