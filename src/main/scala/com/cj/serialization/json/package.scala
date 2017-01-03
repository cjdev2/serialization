package com.cj.serialization

package object json {

  import argonaut._
  import Argonaut._

  implicit object SerializableJson extends Serializable[Json] {
    def serialize(t: Json): Array[Byte] =
      implicitly[Serializable[String]].serialize(t.nospaces)
  }

  implicit object DeserializableJson extends Deserializable[Json] {
    def deserialize(bytes: Array[Byte]): Option[Json] =
      implicitly[Deserializable[String]].deserialize(bytes)
        .flatMap(optStr =>
          Parse.parse(optStr).fold(_ => None, json => Some(json))
        )
  }

  trait JsonSerializer[T] extends Serializable[T] with Deserializable[T] {
    def toJson(t: T): Json
    def fromJson(json: Json): Option[T]

    final def serialize(t: T): Array[Byte] =
      implicitly[Serializable[Json]].serialize(toJson(t))

    final def deserialize(bytes: Array[Byte]): Option[T] =
      implicitly[Deserializable[Json]].deserialize(bytes).flatMap(fromJson)
  }

  def toJson[T: JsonSerializer](t: T): Json =
    implicitly[JsonSerializer[T]].toJson(t)

  def fromJson[T: JsonSerializer](json: Json): Option[T] =
    implicitly[JsonSerializer[T]].fromJson(json)

  def toJsonString[T: JsonSerializer](t: T): String = toJson(t).nospaces

  def toPrettyJsonString[T: JsonSerializer](t: T): String = toJson(t).spaces2

  def fromJsonString[T: JsonSerializer](string: String): Option[T] =
    Parse.parse(string).fold(_ => None, json => Some(json)).flatMap(fromJson[T])

  class JsonSerializerFromCodec[T](codec: CodecJson[T])
    extends JsonSerializer[T] {

    def toJson(t: T): Json = t.asJson(codec)
    def fromJson(json: Json): Option[T] = json.as[T](codec).toOption
  }

  class JsonSerializerFromConversions[T](to: T => Json, from: Json => Option[T])
    extends JsonSerializer[T] {

    def toJson(t: T): Json = to(t)
    def fromJson(json: Json): Option[T] = from(json)
  }
}
