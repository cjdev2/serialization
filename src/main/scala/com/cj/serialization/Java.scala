package com.cj.serialization

import java.util.Optional

/**
  * Java API for `com.cj.serialization`.
  */
object Java {

  /**
    * Abstract class representing the ability to serialize a class `T`
    * into byte arrays.
    *
    * @tparam T The class that can be converted into byte arrays.
    */
  abstract class SerializerJ[T] {
    def serialize(t: T): Array[Byte]
  }

  /**
    * Abstract class representing the ability to deserialize byte
    * arrays into a class `T`.
    *
    * @tparam T The class that can be read from byte arrays.
    */
  abstract class DeserializerJ[T] {
    def deserialize(bytes: Array[Byte]): Optional[T]
  }
}
