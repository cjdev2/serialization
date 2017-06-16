package com.cj.serialization

package object json {

  import argonaut.{DecodeJson, DecodeResult, EncodeJson, HCursor, Json => AJson}
  import scalaz._, Scalaz._

  /**
    * Encapsulates the data required to safely convert back and forth among a
    * type `T` and the types `Json`, `String`, and `Array[Byte]`. Implementors
    * must be thread-safe, must not throw, and must satisfy the contract:
    *
    * {{{
    *   fromJson(toJson(t))       == Some(t)
    *
    *   parseJson(printJson(t))   == Some(t)
    *
    *   parseJson(prettyJson(t))  == Some(t)
    *
    *   deserialize(serialize(t)) == Some(t)
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
    *           [[deserialize]], [[toJson]], [[printJson]], and [[prettyJson]]
    *           prefix methods.
    */
  trait JsonCodec[T] extends Serialize[T] with Deserialize[T] {

    def toJson(t: T): Json
    def fromJson(json: Json): Option[T]

    final def printJson(t: T): String = toJson(t).print
    final def prettyJson(t: T): String = toJson(t).pretty

    final def parseJson(raw: String): Option[T] =
      Json.parse(raw).fold(_ => None, fromJson)

    final def serialize(t: T): Array[Byte] =
      implicitly[Serialize[String]].serialize(printJson(t))

    final def deserialize(bytes: Array[Byte]): Option[T] = for {
      string <- implicitly[Deserialize[String]].deserialize(bytes)
      t <- parseJson(string)
    } yield t
  }

  /**
    * Convert a value of `T` into a value of `Json`.
    *
    * @param t  a value of `t`
    * @tparam T a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return   a representation of `t` as a value of `Json`
    */
  def toJson[T: JsonCodec](t: T): Json =
    implicitly[JsonCodec[T]].toJson(t)

  /**
    * Convert a value of `T` into a JSON string.
    *
    * @param t  a value of `t`
    * @tparam T a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return   a representation of `t` as a JSON string
    */
  def printJson[T: JsonCodec](t: T): String =
    implicitly[JsonCodec[T]].printJson(t)

  /**
    * Convert a value of `T` into a human-readable JSON string.
    *
    * @param t  a value of `t`
    * @tparam T a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return   a representation of `t` as a human-readable JSON string
    */
  def prettyJson[T: JsonCodec](t: T): String =
    implicitly[JsonCodec[T]].prettyJson(t)

  /**
    * Returns the encoded value of `T` from the supplied `Json` value, wrapped
    * as an `Option[T]`, or fails gracefully, returning `None`.
    *
    * @param json any value of `Json`
    * @tparam T   a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return     `Some(t)` if the argument was coherent, otherwise `None`
    */
  def fromJson[T: JsonCodec](json: Json): Option[T] =
    implicitly[JsonCodec[T]].fromJson(json)

  /**
    * Returns the encoded value of `T` from the supplied `String`, wrapped
    * as an `Option[T]`, or fails gracefully, returning `None`.
    *
    * @param string any string
    * @tparam T     a type for which an implicit `JsonSerializer[T]` exists in scope
    * @return       `Some(t)` if the argument was coherent, otherwise `None`
    */
  def parseJson[T: JsonCodec](string: String): Option[T] =
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
      * @param to   a function that converts `T` to `Json`
      * @param from a function that attempts to convert `Json` to `T`
      * @tparam T   The class for which you would like implementations of the
      *             [[serialize]], [[toJson]], [[printJson]], and
      *             [[prettyJson]] prefix methods
      */
    def apply[T](to: T => Json, from: Json => Option[T]): JsonCodec[T] =
      new JsonCodec[T] {
        def toJson(t: T): Json = to(t)
        def fromJson(json: Json): Option[T] = from(json)
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

        def fromJson(json: Json): Option[T] =
          JsonS.toArgonaut(json).as[T](argoCodec).fold(
            failure = (_, _) => None,
            value = t => safely(t)
          )
      }

    /**
      * Implementation of [[JsonCodec]] for `Json`
      */
    implicit object JsonCodecJson extends JsonCodec[Json] {
      def toJson(t: Json): Json = t
      def fromJson(json: Json): Option[Json] = safely(json)
    }

    /**
      * Class implementing [[JsonCodec]] for 'List[T]' whenever `T` has an
      * instances of [[JsonCodec]] in implicit scope.
      *
      * @param codec A `JsonCodec` for the type `T`
      * @tparam T    A target/source type for the serializer.
      * @return      A `JsonCodec` for `List[T]` utilizing the implicit codec.
      */
    implicit def jsonCodecList[T](
                                   implicit codec: JsonCodec[T]
                                 ): JsonCodec[List[T]] =
      new JsonCodec[List[T]] {

        def toJson(ts: List[T]): Json =
          Json.array(ts.map(codec.toJson))

        def fromJson(json: Json): Option[List[T]] = json.array.flatMap
          { jArr => jArr.map(json => codec.fromJson(json)).sequence }
      }

    /**
      * Class implementing [[JsonCodec]] for 'List[T]' whenever `T` has an
      * instances of [[JsonCodec]] in implicit scope.
      *
      * @param codec A `JsonCodec` for the type `T`
      * @tparam T    A target/source type for the serializer.
      * @return      A `JsonCodec` for `Map[String, T]` utilizing the implicit
      *              codec.
      */
    implicit def jsonCodecMap[T](
                                  implicit codec: JsonCodec[T]
                                ): JsonCodec[Map[String, T]] =
      new JsonCodec[Map[String, T]] {
        def toJson(t: Map[String, T]): Json = Json.assoc(t.mapValues(codec.toJson))
        def fromJson(json: Json): Option[Map[String, T]] = json.assoc.flatMap
          { jObj => jObj.mapValues(json => codec.fromJson(json)).sequence }
      }

    /**
      * Class implementing [[JsonCodec]] for 'List[T]' whenever `T` has an
      * instances of [[JsonCodec]] in implicit scope. JSON decodable as values
      * of `T` are identified with `Some` values of `Option[T]`. A JSON `null`
      * literal is identified with the `None` value of `Option[T]`.
      *
      * @param codec A `JsonCodec` for the type `T`
      * @tparam T    A target/source type for the serializer.
      * @return      A `JsonCodec` for `Option[T]` utilizing the implicit codec.
      */
    implicit def jsonCodecOption[T](
                                     implicit codec: JsonCodec[T]
                                   ): JsonCodec[Option[T]] =
      new JsonCodec[Option[T]] {
        def toJson(t: Option[T]): Json = t.fold(Json.nul)(codec.toJson)
        def fromJson(json: Json): Option[Option[T]] =
          codec.fromJson(json) match {
            case None => json.nul match {
              case None => None
              case Some(_) => Some(None)
            }
            case Some(t) => Some(Some(t))
          }
      }
  }

  /**
    * Used with Argonaut codec generation.
    */
  implicit def encodeJson[T](
                              implicit codec: JsonCodec[T]
                            ): EncodeJson[T] =
    new EncodeJson[T] {
      def encode(a: T): AJson = JsonS.toArgonaut(codec.toJson(a))
    }

  /**
    * Used with Argonaut codec generation.
    */
  implicit def decodeJson[T](
                              implicit codec: JsonCodec[T]
                            ): DecodeJson[T] =
    new DecodeJson[T] {
      def decode(hcursor: HCursor): DecodeResult[T] =
        codec.fromJson(JsonS.fromArgonaut(hcursor.cursor.focus)) match {
          case None => DecodeResult.fail("Decode failure!", hcursor.history)
          case Some(a) => DecodeResult.ok(a)
        }
    }
}
