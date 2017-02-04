package com.cj.serialization

package object json {

  /**
    * A type used to represent JSON abstract syntax trees in native Scala, with
    * type-safe constructors and extractors.
    */
  type Json = argonaut.Json

  /**
    * A type used internally by Argonaut that encapsulates a strategy for
    * decoding `Json` to `T`
    *
    * @tparam T
    */
  type DecodeJson[T] = argonaut.DecodeJson[T]

  /**
    * A type used internally by Argonaut that encapsulates a strategy for
    * encoding `T` to `Json`
    *
    * @tparam T
    */
  type EncodeJson[T] = argonaut.EncodeJson[T]

  /**
    * A type used by Argonaut that encapsulate a strategy for encoding and
    * decoding between `T` and `Json`.
    *
    * @tparam T
    */
  type CodecJson[T] = argonaut.CodecJson[T]

  /**
    * Encapsulates the data required to safely convert back and forth among a
    * type `T` and the types `Json`, `String`, and `Array[Byte]`. Implementors
    * should be thread-safe, should not throw, and must satisfy the contract:
    *
    * {{{
    *   fromJson(toJson(t))                   == Some(t)
    *
    *   fromJsonString(toJsonString(t))       == Some(t)
    *
    *   fromJsonString(toPrettyJsonString(t)) == Some(t)
    *
    *   deserialize(serialize(t))             == Some(t)
    *
    *   fromJson(json).map(toJson).flatMap(fromJson)
    *     == fromJson(json)
    *
    *   fromJsonString(string).map(toJsonString).flatMap(fromJsonString)
    *     == fromJsonString(string)
    *
    *   fromJsonString(string).map(toPrettyJsonString).flatMap(fromJsonString)
    *     == fromJsonString(string)
    *
    *   deserialize(bytes).map(serialize).flatMap(deserialize)
    *     == deserialize(bytes)
    * }}}
    *
    * @tparam T The class for which you are implementing the [[serialize]],
    *           [[toJson]], [[toJsonString]], and [[toPrettyJsonString]]
    *           prefix methods
    */
  trait JsonSerializer[T] extends Serializable[T] with Deserializable[T] {

    def toJson(t: T): Json
    def fromJson(json: Json): Option[T]

    final def toJsonString(t: T): String = toJson(t).nospaces
    final def toPrettyJsonString(t: T): String = toJson(t).spaces2

    final def fromJsonString(string: String): Option[T] = for {
      json <- argonaut.Parse.parse(string).fold(_ => None, json => Option(json))
      t <- fromJson(json)
    } yield t

    final def serialize(t: T): Array[Byte] =
      implicitly[Serializable[String]].serialize(toJsonString(t))

    final def deserialize(bytes: Array[Byte]): Option[T] = for {
      string <- implicitly[Deserializable[String]].deserialize(bytes)
      t <- fromJsonString(string)
    } yield t
  }

  /**
    * Convert a value of `T` into a value of `Json`.
    *
    * @param t a value of `t`
    * @tparam T a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return a representation of `t` as a value of `Json`
    */
  def toJson[T: JsonSerializer](t: T): Json =
    implicitly[JsonSerializer[T]].toJson(t)

  /**
    * Convert a value of `T` into a JSON string.
    *
    * @param t a value of `t`
    * @tparam T a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return a representation of `t` as a JSON string
    */
  def toJsonString[T: JsonSerializer](t: T): String =
    implicitly[JsonSerializer[T]].toJsonString(t)

  /**
    * Convert a value of `T` into a human-readable JSON string.
    *
    * @param t a value of `t`
    * @tparam T a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return a representation of `t` as a human-readable JSON string
    */
  def toPrettyJsonString[T: JsonSerializer](t: T): String =
    implicitly[JsonSerializer[T]].toPrettyJsonString(t)

  /**
    * Returns the encoded value of `T` from the supplied `Json` value, wrapped
    * as an `Option[T]`, or fails gracefully, returning `None`.
    *
    * @param json any value of `Json`
    * @tparam T a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return `Some(t)` if the argument was coherent, otherwise `None`
    */
  def fromJson[T: JsonSerializer](json: Json): Option[T] =
    implicitly[JsonSerializer[T]].fromJson(json)

  /**
    * Returns the encoded value of `T` from the supplied `String`, wrapped
    * as an `Option[T]`, or fails gracefully, returning `None`.
    *
    * @param string any string
    * @tparam T a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return `Some(t)` if the argument was coherent, otherwise `None`
    */
  def fromJsonString[T: JsonSerializer](string: String): Option[T] =
    implicitly[JsonSerializer[T]].fromJsonString(string)

  /**
    * Implementation of [[JsonSerializer]] for `Json`
    */
  implicit object JsonSerializerJson extends JsonSerializer[Json] {
    def toJson(t: Json): Json = t
    def fromJson(json: Json): Option[Json] = Option(json)
  }

  /**
    * Class implementing [[JsonSerializer]] for 'List[T]' whenever `T` has
    * instances of `EncodeJson` and `DecodeJson` in implicit scope. No user
    * invocation is necessary.
    *
    * @param encoderT
    * @param decoderT
    * @tparam T
    * @return
    */
  implicit def jsonSerializerList[T](
                                      implicit
                                      encoderT : EncodeJson[T],
                                      decoderT : DecodeJson[T]
                                    ): JsonSerializer[List[T]] =
    new JsonSerializer[List[T]] {

      import scalaz._, Scalaz._

      def toJson(ts: List[T]): Json =
        argonaut.Json.jArray(ts.map(encoderT.encode))

      def fromJson(json: Json): Option[List[T]] =
        json.array.flatMap(_.map(decoderT.decodeJson(_).toOption).sequence)
    }

  /**
    * Create a `JsonSerializer[T]` by supplying a `CodecJson[T]`. The invocation
    * is almost always boilerplate, consisting of routine wiring, as a
    * `CodecJson[T]` can be generated for any case class by supplying the
    * constructor, extractor, and names of the field.
    *
    * For example:
    * {{{
    *   case class Foo(bar: Int, baz: Char)
    *
    *   val fooCodec = Argonaut.casecodec2(Foo.apply, Foo.unapply)("bar", "baz")
    *
    *   object FooS extends JsonSerializerFromCodec[Foo](fooCodec)
    * }}}
    *
    * @param codec a codec that argonaut will use when encoding/decoding `Json`
    * @tparam T The class for which you would like implementations of the
    *           [[serialize]], [[toJson]], [[toJsonString]], and
    *           [[toPrettyJsonString]] prefix methods
    */
  class JsonSerializerFromCodec[T](codec: CodecJson[T])
    extends JsonSerializer[T]
      with EncodeJson[T]
      with DecodeJson[T] {

    def toJson(t: T): Json = argonaut.Argonaut.ToJsonIdentity(t).asJson(codec)
    def fromJson(json: Json): Option[T] = json.as[T](codec).toOption

    def encode(a: T): Json = argonaut.Argonaut.ToJsonIdentity(a).asJson(codec)
    def decode(c: argonaut.HCursor): argonaut.DecodeResult[T] = c.as[T](codec)
    def getCodec: CodecJson[T] = codec
  }

  /**
    * Create a `JsonSerializer[T]` by supplying your own converter functions.
    * The functions supplied must satisfy the contract:
    * {{{
    *   from(to(t))                      == Some(t)
    *   from(json).map(to).flatMap(from) == from(json)
    * }}}
    *
    * @param to a function that converts `T` to `Json`
    * @param from a function that attempts to convert `Json` to `T`
    * @tparam T The class for which you would like implementations of the
    *           [[serialize]], [[toJson]], [[toJsonString]], and
    *           [[toPrettyJsonString]] prefix methods
    */
  class JsonSerializerFromConverters[T](to: T => Json, from: Json => Option[T])
    extends JsonSerializer[T] {

    def toJson(t: T): Json = to(t)
    def fromJson(json: Json): Option[T] = from(json)
  }

  /**
    * Convenience methods for manipulating `Option[Json]`.
    */
  implicit class JsonOp(x: Option[Json]) {

    /**
      * If 'this' contains a Json and it is a JSON object,
      * attempts to lookup the value at the provided key.
      */
    def ~>(key: String): Option[Json] =
      x.flatMap(_.assoc).flatMap(_.toMap.get(key))

    /**
      * If 'this' contains a Json and it is a JSON array,
      * attempts to lookup the value at the privided index.
      */
    def ~>(n: Int): Option[Json] =
      x.flatMap(_.array).flatMap(_.lift(n))

    /**
      * If 'this' contains a Json and it is a JSON object,
      * returns the object represented as a `Map`.
      */
    def obj: Option[Map[String, Json]] =
      x.flatMap(_.assoc).map(_.toMap)

    /**
      * If 'this' contains a Json and it is a JSON array,
      * returns the array represented as a `List`.
      */
    def arr: Option[List[Json]] =
      x.flatMap(_.array)

    /**
      * If 'this' contains a Json and it is a JSON number,
      * attempts to represent the value as a `Long`.
      */
    def long: Option[Long] =
      x.flatMap(_.number).flatMap(_.toLong)

    /**
      * If 'this' contains a Json and it is a JSON number,
      * attempts to represent the value as a `Double`.
      */
    def double: Option[Double] =
      x.flatMap(_.number).flatMap(_.toDouble)

    /**
      * If 'this' contains a Json and it is aa JSON string,
      * returns the string as a `String`.
      */
    def string: Option[String] =
      x.flatMap(_.string)

    /**
      * If 'this' contains a Json and it is a JSON 'true' or 'false' literal,
      * returns an appropriate representation as a `Boolean`.
      */
    def boolean: Option[Boolean] =
      x.flatMap(_.bool)

    /**
      * If 'this' contains a Json and it is the JSON 'null' literal,
      * returns `Some({})`, otherwise returns `None`.
      */
    def nullLiteral: Option[Unit] =
      x.map(_.isNull)
        .flatMap(_ match {
          case false => None
          case true => Some({})
        })
  }
}
