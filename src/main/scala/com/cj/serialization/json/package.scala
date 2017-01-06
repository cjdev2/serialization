package com.cj.serialization

package object json {

  import argonaut._
  import Argonaut._

  trait JsonSerializer[T] extends Serializable[T] with Deserializable[T] {
    def toJson(t: T): Json
    def fromJson(json: Json): Option[T]

    final def toJsonString(t: T): String = this.toJson(t).nospaces
    final def toPrettyJsonString(t: T): String = this.toJson(t).spaces2

    final def fromJsonString(string: String): Option[T] = for {
      json <- Parse.parse(string).fold(_ => None, json => Some(json))
      t <- this.fromJson(json)
    } yield t

    final def serialize(t: T): Array[Byte] =
      implicitly[Serializable[String]].serialize(this.toJsonString(t))

    final def deserialize(bytes: Array[Byte]): Option[T] = for {
      string <- implicitly[Deserializable[String]].deserialize(bytes)
      t <- this.fromJsonString(string)
    } yield t
  }

  def toJson[T: JsonSerializer](t: T): Json =
    implicitly[JsonSerializer[T]].toJson(t)

  def toJsonString[T: JsonSerializer](t: T): String =
    implicitly[JsonSerializer[T]].toJsonString(t)

  def toPrettyJsonString[T: JsonSerializer](t: T): String =
    implicitly[JsonSerializer[T]].toPrettyJsonString(t)

  def fromJson[T: JsonSerializer](json: Json): Option[T] =
    implicitly[JsonSerializer[T]].fromJson(json)

  def fromJsonString[T: JsonSerializer](string: String): Option[T] =
    implicitly[JsonSerializer[T]].fromJsonString(string)

  implicit object JsonSerializerJson extends JsonSerializer[Json] {
    def toJson(t: Json): Json = t
    def fromJson(json: Json): Option[Json] = Some(json)
  }

  class JsonSerializerFromCodec[T](codec: CodecJson[T])
    extends JsonSerializer[T]
    with EncodeJson[T]
    with DecodeJson[T] {

    def toJson(t: T): Json = t.asJson(codec)
    def fromJson(json: Json): Option[T] = json.as[T](codec).toOption

    def encode(a: T): Json = a.asJson(codec)
    def decode(c: HCursor): DecodeResult[T] = c.as[T](codec)
    def getCodec: CodecJson[T] = codec
  }

  class JsonSerializerFromConverters[T](to: T => Json, from: Json => Option[T])
    extends JsonSerializer[T] {

    def toJson(t: T): Json = to(t)
    def fromJson(json: Json): Option[T] = from(json)
  }
}
