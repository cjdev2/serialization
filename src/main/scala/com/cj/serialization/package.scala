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
      def serialize(t: String): Array[Byte] = t.toCharArray.map(_.toByte)
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
    * These instances are only accessible if the user does not provide their own
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
        bytes.map(_.toChar).mkString
      )
    }

    private def asString(bytes: Array[Byte]): Option[String] =
      deserialize[String](bytes)

    implicit object DeserializableBoolean extends Deserializable[Boolean] {
      def deserialize(bytes: Array[Byte]): Option[Boolean] =
        asString(bytes) match {
          case Some("false") => Some(false)
          case Some("true") => Some(true)
          case _ => None
        }
    }

    implicit object DeserializableByte extends Deserializable[Byte] {
      def deserialize(bytes: Array[Byte]): Option[Byte] = {
        if (bytes.length == 1)
          Some(bytes(0))
        else
          None
      }
    }

    implicit object DeserializableChar extends Deserializable[Char] {
      def deserialize(bytes: Array[Byte]): Option[Char] = {
        if (bytes.length == 1)
          scala.util.Try(bytes(0).toChar).toOption
        else
          None
      }
    }

    implicit object DeserializableUnit extends Deserializable[Unit] {
      def deserialize(bytes: Array[Byte]): Option[Unit] =
        asString(bytes) match {
          case Some("()") => Some(Unit)
          case _ => None
        }
    }

    class DeserializableAnyVal[T <: AnyVal](f: String => T)
      extends Deserializable[T] {
      def deserialize(bytes: Array[Byte]): Option[T] = {
        asString(bytes).flatMap(x => scala.util.Try(f(x)).toOption)
      }
    }

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
