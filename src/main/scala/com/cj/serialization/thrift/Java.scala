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

  def serializeThrift[T <: Thrift](t: T): Array[Byte] =
    new TSerializer().serialize(t)

  class ThriftSerializeJ[T <: Thrift] extends SerializeJ[T] {
    def serialize(t: T): Array[Byte] = serializeThrift[T](t)
  }

  def deserializeThrift[T <: Thrift](
                                      prototype: T,
                                      bytes: Array[Byte]
                                    ): Optional[T] =
    safely {
      val spud = prototype.deepCopy.asInstanceOf[T]
      new TDeserializer().deserialize(spud, bytes)
      spud
    }.fold(Optional.empty[T])(Optional.of)

  class ThriftDeserializeJ[T <: Thrift](prototype: T) extends DeserializeJ[T] {
    private val spud = prototype.deepCopy.asInstanceOf[T]
    def deserialize(bytes: Array[Byte]): Optional[T] =
      deserializeThrift[T](spud, bytes)
  }
}
