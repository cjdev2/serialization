package com.cj.serialization
package json

import argonaut.{Json => AJson, JsonObject}
import traversals._

case class Json private[json] (private[json] val aJson: AJson) {

  def fold[X](
               withNull: => X,
               withBoolean: Boolean => X,
               withNumber: BigDecimal => X,
               withString: String => X,
               withArray: List[X] => X,
               withAssoc: Map[String, X] => X
             ): X = {

    def recurse(json: Json) =
      json.fold(withNull, withBoolean, withNumber, withString, withArray, withAssoc)

    aJson.fold(
      withNull, withBoolean, j => withNumber(j.toBigDecimal), withString,
      argArr => withArray(argArr.map(aj => recurse(Json(aj)))),
      argObj => withAssoc(argObj.toMap.map{ case (k, v) => (k, recurse(Json(v))) })
    )
  }

  def nul: Option[Unit] = if (aJson.isNull) Some(()) else None

  def bool: Option[Boolean] = aJson.bool

  def number: Option[BigDecimal] = aJson.number.map(_.toBigDecimal)

  def long: Option[Long] = aJson.number.flatMap(_.toLong)

  def double: Option[Double] = aJson.number.flatMap(_.toDouble)

  def string: Option[String] = aJson.string

  def array: Option[List[Json]] = aJson.array.map(_.map(Json(_)))

  def assoc: Option[Map[String, Json]] =
    aJson.obj.map(_.toMap.map { case (k, v) => (k, Json(v)) })

  def ~>(key: String): Option[Json] =
    assoc.flatMap(_.get(key))

  def ~>(key: Int): Option[Json] =
    ~>(key.toString) orElse array.flatMap(_.lift(key))

  def ><[A](f: Json => Option[A]): Option[List[A]] =
    array.flatMap(_.traverse(f))

  def <>[A](z: A)(f: (A, Json) => Option[A]): Option[A] = array.flatMap {
    _.foldLeft(Option(z)) { (aOp, j) => aOp.flatMap(a => f(a, j)) }
  }

  def :+(kv: (String, ToJson)): Option[Json] =
    aJson.obj.map(o => Json(AJson.jObject(o :+ (kv._1, kv._2.toJson.aJson))))

  def :+(v: ToJson): Option[Json] =
    aJson.array.map(a => Json(AJson.jArray(a :+ v.toJson.aJson)))

  def print: String = aJson.nospaces

  def pretty: String = aJson.spaces2
}

object Json {

  def nul: Json = Json(AJson.jNull)
  def bool(p: Boolean): Json = Json(AJson.jBool(p))
  def number(n: BigDecimal): Json = Json(AJson.jNumber(n))
  def long(n: Long): Json = Json(AJson.jNumber(n))
  def double(x: Double): Option[Json] = AJson.jNumber(x).map(Json(_))
  def string(s: String): Json = Json(AJson.jString(s))
  def array(arr: List[Json]): Json = Json(AJson.jArray(arr.map(_.aJson)))
  def assoc(obj: Map[String, Json]): Json =
    Json(AJson.jObject(JsonObject.fromTraversableOnce(obj.mapValues(_.aJson))))

  def apply(x: ToJson): Json = x.toJson

  def obj(members: (String, ToJson)*): Json =
    assoc(members.map(x => (x._1, x._2.toJson)).toMap)

  def arr(elements: ToJson*): Json =
    array(elements.map(_.toJson).toList)

  def emptyObj: Json = assoc(Map())

  def emptyArr: Json = array(List())

  def parse(raw: String): Either[String, Json] =
    argonaut.Parse.parse(raw).right.map(Json(_))
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

    def ~>(other: Json): Option[Json] =
      other.aJson.obj.map(a => Json(AJson.jObject(a + (self._1, self._2.aJson))))

    def ~>(other: Option[Json]): Option[Json] = other flatMap ~>
  }
}
