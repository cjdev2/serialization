package com.cj

package object serialization {

  /**
    * Provides a [[serialize]] (prefix) method to class `T` post-hoc, which
    * allow values of `T` to be serialized into byte arrays.
    *
    * [[Serializable]]`[_]` is contravariant, in the sense that if `S` is a
    * subtype of `T`, then `Serializable[T]` is a subtype of `Serializable[S]`.
    * As a result, any implementation of `Serializable[T]` is automatically an
    * implementation of `Serializable[S]`.
    *
    * Implementations must be thread-safe and, when an implementation of
    * [[Deserializable]]`[T]` is in scope, must satisfy the following laws:
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
  trait Serializable[-T] {
    def serialize(t: T): Array[Byte]
  }

  /**
    * Call [[serialize]] on any value `t: T` for which an implicit instance of
    * [[Serializable]]`[T]` exists in scope. If there is no implicit instance of
    * `Serializable[T]`---or if there is more than one implicit instance--- in
    * scope, your code will fail to compile (i.e. there is no risk that
    * ambiguity will lead to a runtime error).
    *
    * @param t The value you are serializing
    * @tparam T The type you are serializing
    * @return An `Array[Byte]` representing `t`
    */
  def serialize[T: Serializable](t: T): Array[Byte] =
    implicitly[Serializable[T]].serialize(t)

  /**
    * Some default instances of [[Serializable]]`[T]` for various `T`s.
    *
    * These instances are only accessible if the user does not provide their own
    * instances. E.g., the user may override the library implementation of
    * `Serializable[String]` simply by creating their own implementation:
    * {{{
    *   implicit object Foo extends Serializable[String] {
    *     def serialize(t: String): Array[Byte] = { ... }
    *   }
    * }}}
    */
  object Serializable {

    implicit object SerializableString extends Serializable[String] {
      def serialize(t: String): Array[Byte] = t.getBytes("UTF-8")
    }

    implicit object SerializableAnyVal extends Serializable[AnyVal] {
      def serialize(t: AnyVal): Array[Byte] = t match {
        case t: Byte => Array(t)
        case _ => implicitly[Serializable[String]].serialize(t.toString)
      }
    }
  }

  /**
    * Provides a [[deserialize]] (prefix) method to `Array[Byte]` post-hoc,
    * which allows byte arrays to be parsed into values of `Option[T]`.
    *
    * [[Deserializable]]`[_]` is covariant, in the sense that if `S` is a
    * subtype of `T`, then `Deserializable[S]` is a subtype of
    * `Deserializable[T]`. As a result, any implementation of
    * `Deserializable[S]` is automatically an implementation of
    * `Deserializable[T]`.
    *
    * Implementations must be thread-safe and, when an implementation of
    * [[Serializable]]`[T]` is in scope, must satisfy the following laws:
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
  trait Deserializable[+T] {
    def deserialize(bytes: Array[Byte]): Option[T]
  }

  /**
    * Call [[deserialize]] on any byte array. If there is no implicit instance
    * of [[Deserializable]]`[T]` or if there is more than one implicit instance,
    * then your code will fail to compile (i.e. there is no risk that ambiguity
    * will lead to a runtime error).
    *
    * @param bytes the bytes you hope to deserialize
    * @tparam T the type you hope to retrieve from `bytes`
    * @return An `Option[T]`, where `None` represents failure to deserialize
    */
  def deserialize[T: Deserializable](bytes: Array[Byte]): Option[T] =
    implicitly[Deserializable[T]].deserialize(bytes)

  /**
    * Some default instances of [[Deserializable]]`[T]` for various `T`s.
    *
    * These instances are accessible only if the user does not provide their own
    * instances. E.g., the user may override the library implementation of
    * `Deserializable[String]` simply by creating their own implementation:
    * {{{
    *   implicit object Foo extends Deserializable[String] {
    *     def deserialize(bytes: Array[Byte]): Option[T] = { ... }
    *   }
    * }}}
    */
  object Deserializable {

    implicit object DeserializableString extends Deserializable[String] {
      def deserialize(bytes: Array[Byte]): Option[String] = Option(
        new String(bytes, "UTF-8")
      )
    }

    implicit object DeserializableByte extends Deserializable[Byte] {

      def deserialize(bytes: Array[Byte]): Option[Byte] = bytes.length match {
        case 1 => Option(bytes.head)
        case _ => None
      }
    }

    class DeserializableAnyVal[T <: AnyVal](parse: String => T)
      extends Deserializable[T] {

      def deserialize(bytes: Array[Byte]): Option[T] =
        implicitly[Deserializable[String]].deserialize(bytes)
          .flatMap(string => scala.util.Try(parse(string)).toOption)
          .flatMap(Option.apply)
    }

    implicit object DeserializableChar
      extends DeserializableAnyVal[Char](
        string => string.length match {
          case 1 => string.head
        }
      )

    implicit object DeserializableBoolean
      extends DeserializableAnyVal[Boolean]({
        case "false" => false
        case "true" => true
      })

    implicit object DeserializableUnit
      extends DeserializableAnyVal[Unit]({
        case "()" => ()
      })

    implicit object DeserializableDouble
      extends DeserializableAnyVal[Double](_.toDouble)

    implicit object DeserializableFloat
      extends DeserializableAnyVal[Float](_.toFloat)

    implicit object  DeserializableInt
      extends DeserializableAnyVal[Int](_.toInt)

    implicit object DeserializableLong
      extends DeserializableAnyVal[Long](_.toLong)

    implicit object DeserializableShort
      extends DeserializableAnyVal[Short](_.toShort)
  }
}
