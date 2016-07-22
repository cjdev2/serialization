package com.cj.serialization

import java.io.ByteArrayOutputStream

import org.apache.avro.Schema
import org.apache.avro.io.{BinaryDecoder, BinaryEncoder, DatumWriter, DecoderFactory, EncoderFactory}
import org.apache.avro.specific.{SpecificDatumReader, SpecificDatumWriter, SpecificRecord}

package object avro {
  type RecordSerializer[T] = T => Array[Byte]
  type RecordDeserializer[T] = Array[Byte] => T

  // make a RecordSerializer for class T, passing through intermediate avro-generated class U
  def mkSerializer[T, U <: SpecificRecord](f: (T => U)): RecordSerializer[T] = {
    val avroSerializer = mkAvroSerializer[U]()
    record => avroSerializer(f(record))
  }

  // make a RecordDeserializer for class T, passing through intermediate avro-generated class U
  def mkDeserializer[T, U >: Null <: SpecificRecord](f: U => T, schema: Schema): RecordDeserializer[Option[T]] = {
    val avroDeserializer = mkAvroDeserializer(schema)
    bytes => {
      val avroRec: U = avroDeserializer(bytes)
      if (avroRec == null) None
      else Some(f(avroRec))
    }
  }

  // make a RecordSerializer for avro-generated class T
  def mkAvroSerializer[T <: SpecificRecord](): RecordSerializer[T] = {
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

  // make a RecordDeserializer for avro-generated class T
  def mkAvroDeserializer[T >: Null <: SpecificRecord](schema: Schema): RecordDeserializer[T] = {
    val reader: SpecificDatumReader[T] = new SpecificDatumReader[T](schema)
    var decoder: BinaryDecoder = DecoderFactory.get().binaryDecoder(Array[Byte](), null)
    bytes => {
      decoder = DecoderFactory.get().binaryDecoder(bytes, decoder)
      reader.read(null, decoder)
    }
  }
}
