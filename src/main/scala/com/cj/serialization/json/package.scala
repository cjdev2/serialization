package com.cj.serialization

package object json {

  import argonaut.{DecodeJson, DecodeResult, EncodeJson, HCursor, Json => AJson}
  import traversals._

  /**
    * Encapsulates the data required to safely convert back and forth among a
    * type `T` and the types `Json`, `String`, and `Array[Byte]`. Implementors
    * must be thread-safe, must not throw, and must satisfy the contract:
    *
    * {{{
    * forAll { (t: T) =>
    *   assert { fromJson(toJson(t)) == Some(t) }
    * }
    *
    * forAll { (j: Json) =>
    *   assert { fromJson(j).map(toJson).flatMap(fromJson) == fromJson(j) }
    * }
    * }}}
    *
    * @tparam T The type for which you are implementing the [[toJson]] and
    *           [[fromJson]] methods.
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
    * Implementation of [[JsonCodec]] for `Json`
    */
  implicit lazy val jsonCodecJson: JsonCodec[Json] = new JsonCodec[Json] {
    def toJson(t: Json): Json = t
    def fromJson(json: Json): Option[Json] = safely(json)
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
          Json(argonaut.Argonaut.ToJsonIdentity(t).asJson(argoCodec))

        def fromJson(json: Json): Option[T] =
          json.aJson.as[T](argoCodec).fold(
            failure = (_, _) => None,
            value = t => safely(t)
          )
      }

    /**
      * Implements [[JsonCodec]] for 'List[T]' whenever `T` has an instances of
      * [[JsonCodec]] in implicit scope.
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

        def fromJson(json: Json): Option[List[T]] =
          json.array.flatMap { _.traverse(codec.fromJson) }
      }

    /**
      * Implements [[JsonCodec]] for 'Map[String, T]' whenever `T` has an
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

        def toJson(t: Map[String, T]): Json =
          Json.assoc(t.mapValues(codec.toJson))

        def fromJson(json: Json): Option[Map[String, T]] =
          json.assoc.flatMap { _.traverse(codec.fromJson) }
      }

    /**
      * Implements [[JsonCodec]] for 'Option[T]' whenever `T` has an instances
      * of [[JsonCodec]] in implicit scope. JSON decodable as values of `T` are
      * identified with `Some` values of `Option[T]`. A JSON `null` literal is
      * identified with the `None` value of `Option[T]`. When used on an object
      * property, absence is considered a parse failure rather than a `None`.
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
            case Some(t) => Some(Some(t))
            case None => json.nul match {
              case Some(_) => Some(None)
              case None => None
            }
          }
      }

    /**
      * Implements [[JsonCodec]] for `Tuple2` whenever the component types
      * have instances of [[JsonCodec]] in implicit scope.
      */
    implicit def jsonCodecTuple2[T1: JsonCodec, T2: JsonCodec]:
    JsonCodec[(T1, T2)] = new JsonCodec[(T1, T2)] {
      import JsonImplicits._
      def toJson(t: (T1, T2)): Json = Json.obj(
        "_1" -> json.toJson[T1](t._1),
        "_2" -> json.toJson[T2](t._2)
      )
      def fromJson(j: Json): Option[(T1, T2)] = for {
        _1 <- (j ~> "_1").flatMap(json.fromJson[T1])
        _2 <- (j ~> "_2").flatMap(json.fromJson[T2])
      } yield (_1, _2)
    }

    /**
      * Implements [[JsonCodec]] for `Tuple3` whenever the component types
      * have instances of [[JsonCodec]] in implicit scope.
      */
    implicit def jsonCodecTuple3[T1: JsonCodec, T2: JsonCodec, T3: JsonCodec]:
    JsonCodec[(T1, T2, T3)] = new JsonCodec[(T1, T2, T3)] {
      import JsonImplicits._
      def toJson(t: (T1, T2, T3)): Json = Json.obj(
        "_1" -> json.toJson[T1](t._1),
        "_2" -> json.toJson[T2](t._2),
        "_3" -> json.toJson[T3](t._3)
      )
      def fromJson(j: Json): Option[(T1, T2, T3)] = for {
        _1 <- (j ~> "_1").flatMap(json.fromJson[T1])
        _2 <- (j ~> "_2").flatMap(json.fromJson[T2])
        _3 <- (j ~> "_3").flatMap(json.fromJson[T3])
      } yield (_1, _2, _3)
    }

    /**
      * Implements [[JsonCodec]] for `Tuple4` whenever the component types
      * have instances of [[JsonCodec]] in implicit scope.
      */
    implicit def jsonCodecTuple4[T1: JsonCodec, T2: JsonCodec, T3: JsonCodec, T4: JsonCodec]:
    JsonCodec[(T1, T2, T3, T4)] = new JsonCodec[(T1, T2, T3, T4)] {
      import JsonImplicits._
      def toJson(t: (T1, T2, T3, T4)): Json = Json.obj(
        "_1" -> json.toJson[T1](t._1),
        "_2" -> json.toJson[T2](t._2),
        "_3" -> json.toJson[T3](t._3),
        "_4" -> json.toJson[T4](t._4)
      )
      def fromJson(j: Json): Option[(T1, T2, T3, T4)] = for {
        _1 <- (j ~> "_1").flatMap(json.fromJson[T1])
        _2 <- (j ~> "_2").flatMap(json.fromJson[T2])
        _3 <- (j ~> "_3").flatMap(json.fromJson[T3])
        _4 <- (j ~> "_4").flatMap(json.fromJson[T4])
      } yield (_1, _2, _3, _4)
    }

    /**
      * Implements [[JsonCodec]] for `Tuple5` whenever the component types
      * have instances of [[JsonCodec]] in implicit scope.
      */
    implicit def jsonCodecTuple5[T1: JsonCodec, T2: JsonCodec, T3: JsonCodec, T4: JsonCodec, T5: JsonCodec]:
    JsonCodec[(T1, T2, T3, T4, T5)] = new JsonCodec[(T1, T2, T3, T4, T5)] {
      import JsonImplicits._
      def toJson(t: (T1, T2, T3, T4, T5)): Json = Json.obj(
        "_1" -> json.toJson[T1](t._1),
        "_2" -> json.toJson[T2](t._2),
        "_3" -> json.toJson[T3](t._3),
        "_4" -> json.toJson[T4](t._4),
        "_5" -> json.toJson[T5](t._5)
      )
      def fromJson(j: Json): Option[(T1, T2, T3, T4, T5)] = for {
        _1 <- (j ~> "_1").flatMap(json.fromJson[T1])
        _2 <- (j ~> "_2").flatMap(json.fromJson[T2])
        _3 <- (j ~> "_3").flatMap(json.fromJson[T3])
        _4 <- (j ~> "_4").flatMap(json.fromJson[T4])
        _5 <- (j ~> "_5").flatMap(json.fromJson[T5])
      } yield (_1, _2, _3, _4, _5)
    }

    /**
      * Implements [[JsonCodec]] for `Tuple6` whenever the component types
      * have instances of [[JsonCodec]] in implicit scope.
      */
    implicit def jsonCodecTuple6[T1: JsonCodec, T2: JsonCodec, T3: JsonCodec, T4: JsonCodec, T5: JsonCodec, T6: JsonCodec]:
    JsonCodec[(T1, T2, T3, T4, T5, T6)] = new JsonCodec[(T1, T2, T3, T4, T5, T6)] {
      import JsonImplicits._
      def toJson(t: (T1, T2, T3, T4, T5, T6)): Json = Json.obj(
        "_1" -> json.toJson[T1](t._1),
        "_2" -> json.toJson[T2](t._2),
        "_3" -> json.toJson[T3](t._3),
        "_4" -> json.toJson[T4](t._4),
        "_5" -> json.toJson[T5](t._5),
        "_6" -> json.toJson[T6](t._6)
      )
      def fromJson(j: Json): Option[(T1, T2, T3, T4, T5, T6)] = for {
        _1 <- (j ~> "_1").flatMap(json.fromJson[T1])
        _2 <- (j ~> "_2").flatMap(json.fromJson[T2])
        _3 <- (j ~> "_3").flatMap(json.fromJson[T3])
        _4 <- (j ~> "_4").flatMap(json.fromJson[T4])
        _5 <- (j ~> "_5").flatMap(json.fromJson[T5])
        _6 <- (j ~> "_6").flatMap(json.fromJson[T6])
      } yield (_1, _2, _3, _4, _5, _6)
    }

    /**
      * Implements [[JsonCodec]] for `Tuple7` whenever the component types
      * have instances of [[JsonCodec]] in implicit scope.
      */
    implicit def jsonCodecTuple7[T1: JsonCodec, T2: JsonCodec, T3: JsonCodec, T4: JsonCodec, T5: JsonCodec, T6: JsonCodec, T7: JsonCodec]:
    JsonCodec[(T1, T2, T3, T4, T5, T6, T7)] = new JsonCodec[(T1, T2, T3, T4, T5, T6, T7)] {
      import JsonImplicits._
      def toJson(t: (T1, T2, T3, T4, T5, T6, T7)): Json = Json.obj(
        "_1" -> json.toJson[T1](t._1),
        "_2" -> json.toJson[T2](t._2),
        "_3" -> json.toJson[T3](t._3),
        "_4" -> json.toJson[T4](t._4),
        "_5" -> json.toJson[T5](t._5),
        "_6" -> json.toJson[T6](t._6),
        "_7" -> json.toJson[T7](t._7)
      )
      def fromJson(j: Json): Option[(T1, T2, T3, T4, T5, T6, T7)] = for {
        _1 <- (j ~> "_1").flatMap(json.fromJson[T1])
        _2 <- (j ~> "_2").flatMap(json.fromJson[T2])
        _3 <- (j ~> "_3").flatMap(json.fromJson[T3])
        _4 <- (j ~> "_4").flatMap(json.fromJson[T4])
        _5 <- (j ~> "_5").flatMap(json.fromJson[T5])
        _6 <- (j ~> "_6").flatMap(json.fromJson[T6])
        _7 <- (j ~> "_7").flatMap(json.fromJson[T7])
      } yield (_1, _2, _3, _4, _5, _6, _7)
    }
  }

  /**
    * Used with Argonaut codec generation.
    */
  implicit def encodeJson[T](implicit codec: JsonCodec[T]): EncodeJson[T] =
    new EncodeJson[T] {
      def encode(a: T): AJson = codec.toJson(a).aJson
    }

  /**
    * Used with Argonaut codec generation.
    */
  implicit def decodeJson[T](implicit codec: JsonCodec[T]): DecodeJson[T] =
    new DecodeJson[T] {
      def decode(hcursor: HCursor): DecodeResult[T] =
        codec.fromJson(Json(hcursor.cursor.focus)) match {
          case None => DecodeResult.fail("Decode failure!", hcursor.history)
          case Some(a) => DecodeResult.ok(a)
        }
    }
}
