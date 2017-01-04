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
        .flatMap(
          optStr => Parse.parse(optStr).fold(_ => None, json => Some(json))
        )
  }

  trait JsonSerializer[T] extends Serializable[T] with Deserializable[T] {
    def toJson(t: T): Json
    // def toJsonString(t: T): String
    // def toPrettyJsonString(t: T): String
    def fromJson(json: Json): Option[T]
    // def fromJsonString(string: String): Option[T]

    final def serialize(t: T): Array[Byte] =
      implicitly[Serializable[Json]].serialize(toJson(t))

    final def deserialize(bytes: Array[Byte]): Option[T] =
      implicitly[Deserializable[Json]].deserialize(bytes).flatMap(fromJson)
  }

//  implicit object JsonSerializerJson extends JsonSerializer[Json] {
//    def toJson(t: Json): Json = t
//    def fromJson(json: Json): Option[Json] = Some(json)
//
//    def serialize(t: Json): Array[Byte] =
//      implicitly[Serializable[String]].serialize(t.nospaces)
//
//    def deserialize(bytes: Array[Byte]): Option[Json] =
//      implicitly[Deserializable[String]].deserialize(bytes)
//        .flatMap(
//          optStr => Parse.parse(optStr).fold(_ => None, json => Some(json))
//        )
//  }

  def toJson[T: JsonSerializer](t: T): Json =
    implicitly[JsonSerializer[T]].toJson(t)

  def toJsonString[T: JsonSerializer](t: T): String =
    // implicitly[JsonSerializer[T]].toJsonString(t)
    toJson(t).nospaces

  def toPrettyJsonString[T: JsonSerializer](t: T): String =
    // implicitly[JsonSerializer[T]].toPrettyJsonString(t)
    toJson(t).spaces2

  def fromJson[T: JsonSerializer](json: Json): Option[T] =
    implicitly[JsonSerializer[T]].fromJson(json)

  def fromJsonString[T: JsonSerializer](string: String): Option[T] =
    // implicitly[JsonSerializer[T]].fromJsonString(string)
    deserialize[Json](serialize(string)).flatMap(fromJson[T])

  class JsonSerializerFromCodec[T](codec: CodecJson[T])
    extends JsonSerializer[T] {

    def toJson(t: T): Json = t.asJson(codec)
    // def toJsonString(t: T): String = t.asJson(codec).nospaces
    // def toPrettyJsonString(t: T): String = t.asJson(codec).spaces2
    def fromJson(json: Json): Option[T] = json.as[T](codec).toOption
    // def fromJsonString(string: String): Option[T] = string.decodeOption[T](codec)
  }

  class JsonSerializerFromConversions[T](
                                          to: T => Json,
                                          from: Json => Option[T]
                                        ) extends JsonSerializer[T] {

    def toJson(t: T): Json = to(t)
    // def toJsonString(t: T): String = ???
    // def toPrettyJsonString(t: T): String = ???
    def fromJson(json: Json): Option[T] = from(json)
    // def fromJsonString(string: String): Option[T] = ???
  }
}
