package com.cj

package object serialization {

  /**
    * Provides a [[serialize]] (prefix) method to class `T` post-hoc, which
    * allow values of `T` to be serialized into byte arrays.
    *
    * [[Serialize]]`[_]` is contravariant, in the sense that if `S` is a
    * subtype of `T`, then `Serializable[T]` is a subtype of `Serializable[S]`.
    * As a result, any implementation of `Serializable[T]` is automatically an
    * implementation of `Serializable[S]`.
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
    * `Serializable[T]`---or if there is more than one implicit instance--- in
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
    * `Serializable[String]` simply by creating their own implementation:
    * {{{
    *   implicit object Foo extends Serializable[String] {
    *     def serialize(t: String): Array[Byte] = { ... }
    *   }
    * }}}
    */
  object Serialize {

    implicit object SerializeString extends Serialize[String] {
      def serialize(t: String): Array[Byte] = t.getBytes("UTF-8")
    }

    implicit object SerializeAnyVal extends Serialize[AnyVal] {
      def serialize(t: AnyVal): Array[Byte] = t match {
        case t: Byte => Array(t)
        case _ => implicitly[Serialize[String]].serialize(t.toString)
      }
    }
  }

  /**
    * Provides a [[deserialize]] (prefix) method to `Array[Byte]` post-hoc,
    * which allows byte arrays to be parsed into values of `Result[T]`.
    *
    * [[Deserialize]]`[_]` is covariant, in the sense that if `S` is a
    * subtype of `T`, then `Deserializable[S]` is a subtype of
    * `Deserializable[T]`. As a result, any implementation of
    * `Deserializable[S]` is automatically an implementation of
    * `Deserializable[T]`.
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
    def deserialize(bytes: Array[Byte]): Result[T]
  }

  /**
    * Call [[deserialize]] on any byte array. If there is no implicit instance
    * of [[Deserialize]]`[T]` or if there is more than one implicit instance,
    * then your code will fail to compile (i.e. there is no risk that ambiguity
    * will lead to a runtime error).
    *
    * @param bytes the bytes you hope to deserialize
    * @tparam T the type you hope to retrieve from `bytes`
    * @return An `Result[T]`, where `None` represents failure to deserialize
    */
  def deserialize[T: Deserialize](bytes: Array[Byte]): Result[T] =
    implicitly[Deserialize[T]].deserialize(bytes)

  /**
    * Some default instances of [[Deserialize]]`[T]` for various `T`s.
    *
    * These instances are accessible only if the user does not provide their own
    * instances. E.g., the user may override the library implementation of
    * `Deserializable[String]` simply by creating their own implementation:
    * {{{
    *   implicit object Foo extends Deserializable[String] {
    *     def deserialize(bytes: Array[Byte]): Result[T] = { ... }
    *   }
    * }}}
    */
  object Deserialize {

    implicit object DeserializeString extends Deserialize[String] {
      def deserialize(bytes: Array[Byte]): Result[String] = Result.safely(
        new String(bytes, "UTF-8")
      )
    }

    implicit object DeserializeByte extends Deserialize[Byte] {
      def deserialize(bytes: Array[Byte]): Result[Byte] = bytes.length match {
        case 1 => Result.safely(bytes.head)
        case _ => Result.failure("DeserializeByte: source was more than one byte")
      }
    }

    class DeserializeAnyVal[T <: AnyVal](parse: String => T)
      extends Deserialize[T] {

      def deserialize(bytes: Array[Byte]): Result[T] =
        implicitly[Deserialize[String]].deserialize(bytes)
          .flatMap(string => Result.safely(parse(string)))
    }

    implicit object DeserializeChar
      extends DeserializeAnyVal[Char](
        string => string.length match {
          case 1 => string.head
        }
      )

    implicit object DeserializeBoolean
      extends DeserializeAnyVal[Boolean]({
        case "false" => false
        case "true" => true
      })

    implicit object DeserializeUnit
      extends DeserializeAnyVal[Unit]({
        case "()" => ()
      })

    implicit object DeserializeDouble
      extends DeserializeAnyVal[Double](_.toDouble)

    implicit object DeserializeFloat
      extends DeserializeAnyVal[Float](_.toFloat)

    implicit object  DeserializeInt
      extends DeserializeAnyVal[Int](_.toInt)

    implicit object DeserializeLong
      extends DeserializeAnyVal[Long](_.toLong)

    implicit object DeserializeShort
      extends DeserializeAnyVal[Short](_.toShort)
  }
}
