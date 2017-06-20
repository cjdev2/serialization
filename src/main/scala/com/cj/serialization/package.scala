package com.cj

package object serialization {

  /**
    * Provides a [[serialize]] (prefix) method to class `T` post-hoc, which
    * allow values of `T` to be serialized into byte arrays.
    *
    * [[Serialize]]`[_]` is contravariant, in the sense that if `S` is a
    * subtype of `T`, then `Serialize[T]` is a subtype of `Serialize[S]`.
    * As a result, any implementation of `Serialize[T]` is automatically an
    * implementation of `Serialize[S]`.
    *
    * Implementations must be thread-safe and, when an implementation of
    * [[Deserialize]]`[T]` is in scope, must satisfy the following laws:
    *
    * {{{
    *   deserialize[T](serialize[T](t)) == Some(t)
    *
    *   deserialize[T](bytes).map(serialize[T]).flatMap(deserialize[T])
    *     == deserialize[T](bytes)
    * }}}
    *
    * @tparam T The class for which you are implementing the [[serialize]]
    *           prefix method
    */
  trait Serialize[-T] {
    def serialize(t: T): Array[Byte]
  }

  /**
    * Call [[serialize]] on any value `t: T` for which an implicit instance of
    * [[Serialize]]`[T]` exists in scope. If there is no implicit instance of
    * `Serialize[T]`---or if there is more than one implicit instance--- in
    * scope, your code will fail to compile (i.e. there is no risk that
    * ambiguity will lead to a runtime error).
    *
    * @param t The value you are serializing
    * @tparam T The type you are serializing
    * @return An `Array[Byte]` representing `t`
    */
  def serialize[T: Serialize](t: T): Array[Byte] =
    implicitly[Serialize[T]].serialize(t)

  /**
    * Some default instances of [[Serialize]]`[T]` for various `T`s.
    *
    * These instances are only accessible if the user does not provide their own
    * instances. E.g., the user may override the library implementation of
    * `Serialize[String]` simply by creating their own implementation:
    * {{{
    *   implicit object Foo extends Serialize[String] {
    *     def serialize(t: String): Array[Byte] = { ... }
    *   }
    * }}}
    */
  object Serialize {

    def apply[T](s: T => Array[Byte]): Serialize[T] =
      new Serialize[T] {
        def serialize(t: T): Array[Byte] = s(t)
      }

    implicit lazy val serializeString: Serialize[String] =
      Serialize(t => t.getBytes("UTF-8"))

    implicit lazy val serializeAnyVal: Serialize[AnyVal] =
      Serialize({
        case t: Byte => Array(t)
        case t => serialize[String](t.toString)
      })
  }

  /**
    * Provides a [[deserialize]] (prefix) method to `Array[Byte]` post-hoc,
    * which allows byte arrays to be parsed into values of `Option[T]`.
    *
    * [[Deserialize]]`[_]` is covariant, in the sense that if `S` is a
    * subtype of `T`, then `Deserialize[S]` is a subtype of
    * `Deserialize[T]`. As a result, any implementation of
    * `Deserialize[S]` is automatically an implementation of
    * `Deserialize[T]`.
    *
    * Implementations must be thread-safe and, when an implementation of
    * [[Serialize]]`[T]` is in scope, must satisfy the following laws:
    *
    * {{{
    *   deserialize[T](serialize[T](t)) == Some(t)
    *
    *   deserialize[T](bytes).map(serialize[T]).flatMap(deserialize[T])
    *     == deserialize[T](bytes)
    * }}}
    *
    * @tparam T The return type of the [[deserialize]] prefix method that you
    *           are implementing
    */
  trait Deserialize[+T] {
    def deserialize(bytes: Array[Byte]): Option[T]
  }

  /**
    * Call [[deserialize]] on any byte array. If there is no implicit instance
    * of [[Deserialize]]`[T]` or if there is more than one implicit instance,
    * then your code will fail to compile (i.e. there is no risk that ambiguity
    * will lead to a runtime error).
    *
    * @param bytes the bytes you hope to deserialize
    * @tparam T the type you hope to retrieve from `bytes`
    * @return An `Option[T]`, where `None` represents failure to deserialize
    */
  def deserialize[T: Deserialize](bytes: Array[Byte]): Option[T] =
    implicitly[Deserialize[T]].deserialize(bytes)

  /**
    * Some default instances of [[Deserialize]]`[T]` for various `T`s.
    *
    * These instances are accessible only if the user does not provide their own
    * instances. E.g., the user may override the library implementation of
    * `Deserialize[String]` simply by creating their own implementation:
    * {{{
    *   implicit object Foo extends Deserialize[String] {
    *     def deserialize(bytes: Array[Byte]): Option[T] = { ... }
    *   }
    * }}}
    */
  object Deserialize {

    def apply[T](d: Array[Byte] => Option[T]): Deserialize[T] =
      new Deserialize[T] {
        def deserialize(bytes: Array[Byte]): Option[T] = d(bytes)
      }

    def kleisli[T: Deserialize, S](k: T => Option[S]): Deserialize[S] =
      Deserialize[S](bs => deserialize[T](bs).flatMap(k))

    def compose[T: Deserialize, S](f: T => S): Deserialize[S] =
      kleisli[T, S](t => safely(f(t)))

    implicit lazy val deserializeString: Deserialize[String] =
      Deserialize(bytes => safely(new String(bytes, "UTF-8")))

    implicit lazy val deserializeByte: Deserialize[Byte] =
      Deserialize(bytes => bytes.length match {
        case 1 => safely(bytes.head)
        case _ => None
      })

    implicit lazy val deserializeChar: Deserialize[Char] =
      compose[String, Char](str => str.length match { case 1 => str.head })

    implicit lazy val deserializeBoolean: Deserialize[Boolean] =
      compose[String, Boolean]({
        case "false" => false
        case "true" => true
      })

    implicit lazy val deserializeUnit: Deserialize[Unit] =
      compose[String, Unit]({ case "()" => () })

    implicit lazy val deserializeDouble: Deserialize[Double] =
      compose[String, Double](_.toDouble)

    implicit lazy val deserializeFloat: Deserialize[Float] =
      compose[String, Float](_.toFloat)

    implicit lazy val deserializeInt: Deserialize[Int] =
      compose[String, Int](_.toInt)

    implicit lazy val deserializeLong: Deserialize[Long] =
      compose[String, Long](_.toLong)

    implicit lazy val deserializeShort: Deserialize[Short] =
      compose[String, Short](_.toShort)
  }

  private[serialization] def safely[T](t: => T): Option[T] =
    scala.util.Try(t).toOption.flatMap(Option.apply)
}
