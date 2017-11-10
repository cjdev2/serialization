package com.cj.serialization
package json

import traversals._
import JsonS._

sealed abstract class Json extends Product with Serializable {

  def fold[X](
               withNull: => X,
               withBoolean: Boolean => X,
               withNumber: BigDecimal => X,
               withString: String => X,
               withArray: List[X] => X,
               withAssoc: Map[String, X] => X
             ): X = {

    val recurse: Json => X =
      _.fold(withNull,withBoolean,withNumber,withString,withArray,withAssoc)

    this match {
      case JNull => withNull
      case JBool(p) => withBoolean(p)
      case JNumber(x) => withNumber(x)
      case JString(x) => withString(x)
      case JArray(x) => withArray(x.map(recurse))
      case JAssoc(x) => withAssoc(x.mapValues(recurse))
    }
  }

  def nul: Option[Unit] = this match {
    case JNull => Some(())
    case _ => None
  }

  def bool: Option[Boolean] = this match {
    case JBool(p) => Some(p)
    case _ => None
  }

  def number: Option[BigDecimal] = this match {
    case JNumber(x) => Some(x)
    case _ => None
  }

  def long: Option[Long] = this match {
    case JNumber(x) => x.toLong match {
      case y if BigDecimal(y) == x => Some(y)
      case _ => None
    }
    case _ => None
  }

  def double: Option[Double] = this match {
    case JNumber(x) => x.toDouble match {
      case y if BigDecimal(y) == x => Some(y)
      case _ => None
    }
    case _ => None
  }

  def string: Option[String] = this match {
    case JString(x) => Some(x)
    case _ => None
  }

  def array: Option[List[Json]] = this match {
    case JArray(x) => Some(x)
    case _ => None
  }

  def assoc: Option[Map[String, Json]] = this match {
    case JAssoc(x) => Some(x)
    case _ => None
  }

  def ~>(key: String): Option[Json] = this match {
    case JAssoc(x) => x.get(key)
    case _ => None
  }

  def ~>(key: Int): Option[Json] = this match {
    case JAssoc(x) => x.get(key.toString)
    case JArray(x) => x.lift(key)
    case _ => None
  }

  def ><[A](f: Json => Option[A]): Option[List[A]] =
    this.array.flatMap(_.traverse(f))

  def <>[A](z: A)(f: (A, Json) => Option[A]): Option[A] = this.array.flatMap {
    _.foldLeft(Option(z)) { (aOp, j) => aOp.flatMap(a => f(a, j)) }
  }

  def print: String = JsonS.print(this)

  def pretty: String = JsonS.pretty(this)
}

object Json {

  def nul: Json = JNull
  def bool(p: Boolean): Json = JBool(p)
  def number(n: BigDecimal): Json = JNumber(n)
  def long(n: Long): Json = JNumber(BigDecimal(n))
  def double(x: Double): Json = JNumber(BigDecimal(x))
  def string(s: String): Json = JString(s)
  def array(arr: List[Json]): Json = JArray(arr)
  def assoc(obj: Map[String, Json]): Json = JAssoc(obj)

  def apply(x: ToJson): Json = x.toJson

  def obj(members: (String, ToJson)*): Json =
    assoc(members.map(x => (x._1, x._2.toJson)).toMap)

  def arr(elements: ToJson*): Json =
    array(elements.map(_.toJson).toList)

  def emptyObj: Json = assoc(Map())

  def emptyArr: Json = array(List())

  def parse(raw: String): Either[String, Json] = JsonS.parse(raw)
}

sealed trait ToJson {
  def toJson: Json
}

object JsonImplicits {

  implicit class JsonToJson(self: Json) extends ToJson {
    def toJson: Json = self
  }

  implicit class UnitToJson(self: Unit) extends ToJson {
    def toJson: Json = Json.nul
  }

  implicit class BoolToJson(self: Boolean) extends ToJson {
    def toJson: Json = Json.bool(self)
  }

  implicit class BigDecimalToJson(self: BigDecimal) extends ToJson {
    def toJson: Json = Json.number(self)
  }

  implicit class LongToJson(self: Long) extends ToJson {
    def toJson: Json = Json.number(BigDecimal(self))
  }

  implicit class IntToJson(self: Int) extends ToJson {
    def toJson: Json = Json.number(BigDecimal(self))
  }

  implicit class DoubleToJson(self: Double) extends ToJson {
    def toJson: Json = Json.number(BigDecimal(self))
  }

  implicit class FloatToJson(self: Float) extends ToJson {
    def toJson: Json = Json.number(BigDecimal(self))
  }

  implicit class StringToJson(self: String) extends ToJson {
    def toJson: Json = Json.string(self)
  }

  implicit class OptionToJson[A <: ToJson](self: Option[A]) extends ToJson {
    def toJson: Json = self.fold(Json.nul)(_.toJson)
  }

  implicit class ListToJson[A <: ToJson](self: List[A]) extends ToJson {
    def toJson: Json = Json.array(self.map(_.toJson))
  }

  implicit class MapToJson[A <: ToJson](self: Map[String, Json]) extends ToJson {
    def toJson: Json = Json.assoc(self.mapValues(_.toJson))
  }

  implicit class JsonOp(val self: Option[Json]) extends AnyVal {

    def nul: Option[Unit] = self.flatMap(_.nul)
    def bool: Option[Boolean] = self.flatMap(_.bool)
    def number: Option[BigDecimal] = self.flatMap(_.number)
    def long: Option[Long] = self.flatMap(_.long)
    def double: Option[Double] = self.flatMap(_.double)
    def string: Option[String] = self.flatMap(_.string)
    def array: Option[List[Json]] = self.flatMap(_.array)
    def assoc: Option[Map[String, Json]] = self.flatMap(_.assoc)

    def ~>(key: String): Option[Json] =
      self.flatMap(_.~>(key))

    def ~>(key: Int): Option[Json] =
      self.flatMap(_.~>(key))

    def ><[A](f: Json => Option[A]): Option[List[A]] =
      self.array.flatMap(_.traverse(f))

    def <>[A](z: A)(f: (A, Json) => Option[A]): Option[A] =
      self.array.flatMap { jsons =>
        jsons.foldLeft(Option(z)) { (aOp, j) => aOp.flatMap(a => f(a, j)) }
      }
  }

  implicit class JsonTraversalList(val self: List[Json]) extends AnyVal {

    def ><[A](f: Json => Option[A]): Option[List[A]] =
      self.traverse(_ >< f).map(_.flatten)

    def <>[A](z: A)(f: (A, Json) => Option[A]): Option[A] =
      self.foldLeft(Option(z)) { (aOp, j) => aOp.flatMap(a => f(a, j)) }
  }

  implicit class JsonTraversalListOp(val self: Option[List[Json]]) extends AnyVal {

    def ><[A](f: Json => Option[A]): Option[List[A]] =
      self.flatMap { _.traverse(_ >< f).map(_.flatten) }

    def <>[A](z: A)(f: (A, Json) => Option[A]): Option[A] =
      self.flatMap { x =>
        x.foldLeft(Option(z)) { (aOp, j) => aOp.flatMap(a => f(a, j)) }
      }
  }

  implicit class JsonPair(val self: (String, Json)) extends AnyVal {

    def ~>(other: Json): Option[Json] = other match {
      case JAssoc(obj) => Some(JAssoc(obj + self))
      case _ => None
    }

    def ~>(other: Option[Json]): Option[Json] = other flatMap ~>
  }
}

private[json] object JsonS {

  case object JNull extends Json
  case class JBool(get: Boolean) extends Json
  case class JNumber(get: BigDecimal) extends Json
  case class JString(get: String) extends Json
  case class JArray(get: List[Json]) extends Json
  case class JAssoc(get: Map[String, Json]) extends Json

  import argonaut.{Json => AJson, JsonObject}

  def fromArgonaut(ajson: AJson): Json = ajson.fold[Json](
    jsonNull = Json.nul,
    jsonBool = p => Json.bool(p),
    jsonNumber = n => Json.number(n.toBigDecimal),
    jsonString = s => Json.string(s),
    jsonArray = array => Json.array(array.map(fromArgonaut)),
    jsonObject = assoc => Json.assoc(
      assoc.toMap.map({case (k, v) => (k, fromArgonaut(v))}))
  )

  def toArgonaut(json: Json): AJson = json.fold[AJson](
    withNull = AJson.jNull,
    withBoolean = p => AJson.jBool(p),
    withNumber = n => AJson.jNumber(n),
    withString = s => AJson.jString(s),
    withArray = array => AJson.jArray(array),
    withAssoc = assoc =>
      AJson.jObject(JsonObject.fromTraversableOnce(assoc))
  )

  def print(json: Json): String = toArgonaut(json).nospaces

  def pretty(json: Json): String = toArgonaut(json).spaces2

  def parse(raw: String): Either[String, Json] =
    argonaut.Parse.parse(raw).fold(
      msg => Left(msg),
      ajson => Right(fromArgonaut(ajson))
    )
}
