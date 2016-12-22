package com.cj.serialization

package object json {

  sealed trait JsonValue
  sealed trait JsonConstant extends JsonValue
  case object JsonNull extends JsonConstant
  case object JsonFalse extends JsonConstant
  case object JsonTrue extends JsonConstant
  case class JsonNumber(num: Double) extends JsonValue
  case class JsonString(str: String) extends JsonValue
  case class JsonArray(arr: Seq[JsonValue]) extends JsonValue
  case class JsonObject(obj: Map[String, JsonValue]) extends JsonValue

  implicit object SerializableJsonValue extends Serializable[JsonValue] {
    def serialize(t: JsonValue): Array[Byte] =
      jsonToString(t).toCharArray.map(_.toByte)
  }

  implicit object DeserializableJsonValue extends Deserializable[JsonValue] {
    def deserialize(bytes: Array[Byte]): Option[JsonValue] =
      stringToJson(bytes.map(_.toChar).mkString)
  }

  class JsonSerializer[T](f: T => JsonValue) extends Serializable[T] {
    def serialize(t: T): Array[Byte] =
      implicitly[Serializable[JsonValue]].serialize(f(t))
  }

  class JsonDeserializer[T](f: JsonValue => T) extends Deserializable[T] {
    def deserialize(bytes: Array[Byte]): Option[T] =
      implicitly[Deserializable[JsonValue]].deserialize(bytes).map(f)
  }

  private def constToString(const: JsonConstant): String = const match {
    case JsonNull => "null"
    case JsonFalse => "false"
    case JsonTrue => "true"
  }

  private def numToString(num: JsonNumber): String =
    num.num.toString.split("\\.") match {
      case Array(str1, "0") => str1
      case _ => num.num.toString
    }

  private def strToString(str: JsonString): String = s""""${str.str}""""

  private def combineWithCommas(strs: Seq[String]): String = strs match {
    case Seq() => ""
    case _ => strs.foldRight("")(_ ++ "," ++ _).init
  }

  private def arrToString(arr: JsonArray): String =
    s"""[${combineWithCommas(arr.arr.map(jsonToString))}]"""

  private def objToString(obj: JsonObject): String = {
    def pairToString = (pair: (String, JsonValue)) =>
      s""""${pair._1}":${jsonToString(pair._2)}"""

    s"""{${combineWithCommas(obj.obj.map(pairToString).toSeq)}}"""
  }

  private def jsonToString(jval: JsonValue): String = jval match {
    case jval: JsonConstant => constToString(jval)
    case jval: JsonString => strToString(jval)
    case jval: JsonNumber => numToString(jval)
    case jval: JsonArray => arrToString(jval)
    case jval: JsonObject => objToString(jval)
  }

  private def stringToJson(jlit: String): Option[JsonValue] = ???
}
