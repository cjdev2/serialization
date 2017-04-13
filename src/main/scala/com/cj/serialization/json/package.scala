package com.cj.serialization

package object json {

  import argonaut.{DecodeJson, DecodeResult, EncodeJson, HCursor, Json => AJson}
  import scalaz._, Scalaz._

  /**
    * Encapsulates the data required to safely convert back and forth among a
    * type `T` and the types `Json`, `String`, and `Array[Byte]`. Implementors
    * should be thread-safe, should not throw, and must satisfy the contract:
    *
    * {{{
    *   fromJson(toJson(t))                   == Some(t)
    *
    *   parseJson(printJson(t))               == Some(t)
    *
    *   parseJson(prettyJson(t))              == Some(t)
    *
    *   deserialize(serialize(t))             == Some(t)
    *
    *   fromJson(json).map(toJson).flatMap(fromJson)
    *     == fromJson(json)
    *
    *   parseJson(string).map(printJson).flatMap(parseJson)
    *     == fromJsonString(string)
    *
    *   parseJson(string).map(prettyJson).flatMap(parseJson)
    *     == fromJsonString(string)
    *
    *   deserialize(bytes).map(serialize).flatMap(deserialize)
    *     == deserialize(bytes)
    * }}}
    *
    * @tparam T The class for which you are implementing the [[serialize]],
    *           [[toJson]], [[printJson]], and [[prettyJson]]
    *           prefix methods
    */
  trait JsonCodec[T] extends Serialize[T] with Deserialize[T] {

    def toJson(t: T): Json
    def fromJson(json: Json): Result[T]

    final def printJson(t: T): String = toJson(t).print
    final def prettyJson(t: T): String = toJson(t).pretty

    final def parseJson(string: String): Result[T] =
      Json.parse(string).flatMap(fromJson)

    final def serialize(t: T): Array[Byte] =
      implicitly[Serialize[String]].serialize(printJson(t))

    final def deserialize(bytes: Array[Byte]): Result[T] = for {
      string <- implicitly[Deserialize[String]].deserialize(bytes)
      t <- parseJson(string)
    } yield t
  }

  /**
    * Convert a value of `T` into a value of `Json`.
    *
    * @param t a value of `t`
    * @tparam T a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return a representation of `t` as a value of `Json`
    */
  def toJson[T: JsonCodec](t: T): Json =
    implicitly[JsonCodec[T]].toJson(t)

  /**
    * Convert a value of `T` into a JSON string.
    *
    * @param t a value of `t`
    * @tparam T a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return a representation of `t` as a JSON string
    */
  def printJson[T: JsonCodec](t: T): String =
    implicitly[JsonCodec[T]].printJson(t)

  /**
    * Convert a value of `T` into a human-readable JSON string.
    *
    * @param t a value of `t`
    * @tparam T a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return a representation of `t` as a human-readable JSON string
    */
  def prettyJson[T: JsonCodec](t: T): String =
    implicitly[JsonCodec[T]].prettyJson(t)

  /**
    * Returns the encoded value of `T` from the supplied `Json` value, wrapped
    * as an `Result[T]`, or fails gracefully, returning `None`.
    *
    * @param json any value of `Json`
    * @tparam T a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return `Some(t)` if the argument was coherent, otherwise `None`
    */
  def fromJson[T: JsonCodec](json: Json): Result[T] =
    implicitly[JsonCodec[T]].fromJson(json)

  /**
    * Returns the encoded value of `T` from the supplied `String`, wrapped
    * as an `Result[T]`, or fails gracefully, returning `None`.
    *
    * @param string any string
    * @tparam T a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return `Some(t)` if the argument was coherent, otherwise `None`
    */
  def parseJson[T: JsonCodec](string: String): Result[T] =
    implicitly[JsonCodec[T]].parseJson(string)

  object JsonCodec {

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
      *           [[serialize]], [[toJson]], [[printJson]], and
      *           [[prettyJson]] prefix methods
      */
    def apply[T](to: T => Json, from: Json => Result[T]): JsonCodec[T] =
      new JsonCodec[T] {
        def toJson(t: T): Json = to(t)
        def fromJson(json: Json): Result[T] = from(json)
      }

    /**
      * Create a [[JsonCodec]][T] by supplying an `argonaut.CodecJson[T]`.
      * The invocation is almost always boilerplate, consisting of routine
      * wiring, as a `CodecJson[T]` can be generated for any case class by
      * supplying the constructor, extractor, and names of the field.
      *
      * For example:
      * {{{
      *   case class Foo(bar: Int, baz: Char)
      *
      *   implicit val fooCodec = JsonCodec(
      *     argonaut.Argonaut.casecodec2(Foo.apply, Foo.unapply)("bar", "baz")
      *   )
      *
      *   /* now convert `Foo`s back and forth with [[Json]] as normal */
      * }}}
      *
      * @param argoCodec a codec that argonaut will use when encoding/decoding [[Json]]
      * @tparam T The class for which you would like implementations of the
      *           [[serialize]], [[deserialize]], [[parseJson]], [[toJson]],
      *           [[fromJson]], [[printJson]], and [[prettyJson]] prefix methods
      */
    def apply[T](argoCodec: argonaut.CodecJson[T]): JsonCodec[T] =
      new JsonCodec[T] {

        def toJson(t: T): Json =
          JsonS.fromArgonaut(argonaut.Argonaut.ToJsonIdentity(t).asJson(argoCodec))

        def fromJson(json: Json): Result[T] =
          JsonS.toArgonaut(json).as[T](argoCodec).fold(
            failure = (s, _) => Result.failure(s),
            value = t => Result.safely(t)
          )
      }

    /**
      * Implementation of [[JsonCodec]] for `Json`
      */
    implicit object JsonCodecJson extends JsonCodec[Json] {
      def toJson(t: Json): Json = t
      def fromJson(json: Json): Result[Json] = Result.safely(json)
    }

    /**
      * Class implementing [[JsonCodec]] for 'List[T]' whenever `T` has
      * instances of `EncodeJson` and `DecodeJson` in implicit scope. No user
      * invocation is necessary.
      *
      * @param codec A JsonSerializer for the type `T`
      * @tparam T       A target/source type for the serializer.
      * @return         A JSON serializer utilizing the implicit encoder/decoder.
      */
    implicit def jsonCodecList[T](
                                   implicit codec: JsonCodec[T]
                                 ): JsonCodec[List[T]] =
      new JsonCodec[List[T]] {

        def toJson(ts: List[T]): Json =
          Json.array(ts.map(codec.toJson))

        def fromJson(json: Json): Result[List[T]] =
          json.array
            .map(jArr => jArr.map(json => codec.fromJson(json)).sequence)
            .getOrElse(Result.failure(s"Json was not an array: $json"))
      }

    implicit def jsonCodecMap[T](
                                  implicit codec: JsonCodec[T]
                                ): JsonCodec[Map[String, T]] =
      new JsonCodec[Map[String, T]] {
        def toJson(t: Map[String, T]): Json = Json.assoc(t.mapValues(codec.toJson))
        def fromJson(json: Json): Result[Map[String, T]] =
          json.assoc
            .map(jObj => jObj.mapValues(json => codec.fromJson(json)).sequence)
            .getOrElse(Result.failure(s"Json was no an object: $json"))
      }
  }

  implicit def encodeJson[T](
                              implicit codec: JsonCodec[T]
                            ): EncodeJson[T] =
    new EncodeJson[T] {
      def encode(a: T): AJson = JsonS.toArgonaut(codec.toJson(a))
    }

  implicit def decodeJson[T](
                              implicit codec: JsonCodec[T]
                            ): DecodeJson[T] =
    new DecodeJson[T] {
      def decode(hcursor: HCursor): DecodeResult[T] =
        codec.fromJson(JsonS.fromArgonaut(hcursor.cursor.focus)).fold(
          withFailure = msg => DecodeResult.fail(msg, hcursor.history),
          withSuccess = a => DecodeResult.ok(a)
        )
    }
}
