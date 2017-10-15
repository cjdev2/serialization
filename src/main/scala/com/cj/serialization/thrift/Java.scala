package com.cj.serialization
package thrift

import java.util.Optional

import scala.language.existentials
import com.cj.serialization.Java._
import org.apache.thrift.{TBase, TDeserializer, TFieldIdEnum, TSerializer}

object Java {

  private type Thrift = TBase[A, B] forSome {
    type B <: TFieldIdEnum
    type A <: TBase[A, B]
  }

  /**
    * Supply an object that is a member of `TBase` to get a Thrift-compliant
    * byte representation.
    */
  def serializeThrift[T <: Thrift](t: T): Array[Byte] =
    new TSerializer().serialize(t)

  /**
    * A class to represent a serializer for the given class `T` extending
    * `TBase`. Strictly speaking, this class is redundant, as
    * `serializeThrift` works generically for any class extending `TBase`.
    */
  class ThriftSerializeJ[T <: Thrift] extends SerializeJ[T] {
    def serialize(t: T): Array[Byte] = serializeThrift[T](t)
  }

  /**
    * Attempts to deserialize the provided `bytes` as a member of the provided
    * `clazz`, which must extend `TBase`.
    */
  def deserializeThrift[T <: Thrift](
                                      clazz: Class[T],
                                      bytes: Array[Byte]
                                    ): Optional[T] =
    safely {
      val spud = clazz.getDeclaredConstructor().newInstance()
      new TDeserializer().deserialize(spud, bytes)
      spud
    }.fold(Optional.empty[T])(Optional.of)

  /**
    * A class to represent a deserializer for the given class `T` extending
    * `TBase`. Strictly speaking, this class is redundant, as
    * `deserializeThrift` works generically for any class extending `TBase`.
    */
  class ThriftDeserializeJ[T <: Thrift](clazz: Class[T]) extends DeserializeJ[T] {
    def deserialize(bytes: Array[Byte]): Optional[T] =
      deserializeThrift[T](clazz, bytes)
  }
}
