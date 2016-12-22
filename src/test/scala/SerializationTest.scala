import org.scalatest.{FlatSpec, Matchers}

class SerializationTest extends FlatSpec with Matchers {

  import com.cj.serialization._

  behavior of "Serializable"

  it should "be open to post-hoc implementations" in {
    // given: a class
    case class Foo(bar: Int, baz: Char)

    // when: we implement `Serializable[Foo]`
    implicit object SerializableFoo extends Serializable[Foo] {
      def serialize(t: Foo): Array[Byte] =
        s"""Foo ${t.bar} ${t.baz}""".toCharArray.map(_.toByte)
    }

    // then: `serialize` should be available for arguments of type `Foo`
    serialize(Foo(0,'c')) should be("Foo 0 c".toCharArray.map(_.toByte))
  }

  behavior of "Deserializable"

  it should "be open to post-hoc implementations" in {
    // given: a class
    case class Foo(bar: Int, baz: Char)

    // when: we implement `Deserialize[Foo]`
    implicit object DeserializableFoo extends Deserializable[Foo] {
      def deserialize(bytes: Array[Byte]): Option[Foo] = {

        val tokens = bytes.map(_.toChar).mkString.split(" ")

        if (tokens.length == 3 && tokens(0) == "Foo") {
          val num = scala.util.Try(tokens(1).toInt).toOption
          val char = tokens(2).toList match {
            case List(c) => Some(c)
            case _ => None
          }
          for {
            n <- num
            c <- char
          } yield Foo(n, c)
        } else None
      }
    }

    // then: `deserialize` should be able to return values of type `Option[Foo]`
    deserialize[Foo](
      "Foo 123 a".toCharArray.map(_.toByte)
    ) should be(Some(Foo(123, 'a')))
  }

  behavior of "String"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = "foo".toCharArray.map(_.toByte)
    val bytes2: Array[Byte] = "".toCharArray.map(_.toByte)

    // when
    val result1: Option[String] = deserialize(bytes1)
    val result2: Option[String] = deserialize(bytes2)

    // then
    result1.isDefined should be(true)
    result2.isDefined should be(true)
  }

  it should "be serializable" in {
    // given
    val string: String = "foo"

    // when
    serialize(string)

    // then
    true
  }

  it should "be reversible" in {
    // given
    val string1: String = "foo"
    val string2: String = ""

    // when
    val result1: Option[String] = deserialize(serialize(string1))
    val result2: Option[String] = deserialize(serialize(string2))

    // then
    result1.get should be(string1)
    result2.get should be(string2)
  }

  behavior of "Boolean"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = "foo".toCharArray.map(_.toByte)
    val bytes2: Array[Byte] = "false".toCharArray.map(_.toByte)
    val bytes3: Array[Byte] = "true".toCharArray.map(_.toByte)

    // when
    val result1: Option[Boolean] = deserialize(bytes1)
    val result2: Option[Boolean] = deserialize(bytes2)
    val result3: Option[Boolean] = deserialize(bytes3)

    // then
    result1.isDefined should be(false)
    result2.isDefined should be(true)
    result3.isDefined should be(true)
  }

  it should "be serializable" in {
    // given
    val bool1: Boolean = false
    val bool2: Boolean = true

    // when
    serialize(bool1)
    serialize(bool2)

    // then
    true
  }

  it should "be reversible" in {
    // given
    val bool1: Boolean = false
    val bool2: Boolean = true

    // when
    val result1: Option[Boolean] = deserialize(serialize(bool1))
    val result2: Option[Boolean] = deserialize(serialize(bool2))

    // then
    result1.get should be(bool1)
    result2.get should be(bool2)
  }

  behavior of "Byte"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = "foo".toCharArray.map(_.toByte)
    val bytes2: Array[Byte] = "f".toCharArray.map(_.toByte)
    val bytes3: Array[Byte] = "".toCharArray.map(_.toByte)

    // when
    val result1: Option[Byte] = deserialize(bytes1)
    val result2: Option[Byte] = deserialize(bytes2)
    val result3: Option[Byte] = deserialize(bytes3)

    // then
    result1.isDefined should be(false)
    result2.isDefined should be(true)
    result3.isDefined should be(false)
  }

  it should "be serializable" in {
    // given
    val byte: Byte = 'f'.toByte

    // when
    serialize(byte)

    // then
    true
  }

  it should "be reversible" in {
    // given
    val byte: Byte = 'f'.toByte

    // when
    val result: Option[Byte] = deserialize(serialize(byte))

    // then
    result.get should be(byte)
  }

  behavior of "Char"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = "foo".toCharArray.map(_.toByte)
    val bytes2: Array[Byte] = "f".toCharArray.map(_.toByte)
    val bytes3: Array[Byte] = "".toCharArray.map(_.toByte)

    // when
    val result1: Option[Char] = deserialize(bytes1)
    val result2: Option[Char] = deserialize(bytes2)
    val result3: Option[Char] = deserialize(bytes3)

    // then
    result1.isDefined should be(false)
    result2.isDefined should be(true)
    result3.isDefined should be(false)
  }

  it should "be serializable" in {
    // given
    val char: Byte = 'f'

    // when
    serialize(char)

    // then
    true
  }

  it should "be reversible" in {
    // given
    val char: Byte = 'f'

    // when
    val result: Option[Char] = deserialize(serialize(char))

    // then
    result.get should be(char)
  }

  behavior of "Double"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = serialize(4567.567)
    val bytes2: Array[Byte] = serialize(456)
    val bytes3: Array[Byte] = serialize(-4567.567)

    // when
    val result1: Option[Double] = deserialize(bytes1)
    val result2: Option[Double] = deserialize(bytes2)
    val result3: Option[Double] = deserialize(bytes3)

    // then
    result1.isDefined should be(true)
    result2.isDefined should be(true)
    result3.isDefined should be(true)
  }

  it should "be serializable" in {
    // given
    val doub: Double = 3456.3456

    // when
    serialize(doub)

    // then
    true
  }

  it should "be reversible" in {
    // given
    val doub1: Double = 4567.567
    val doub2: Double = 456
    val doub3: Double = -4567.567
    val doub4: Double =
      scala.math.pow(scala.math.E, scala.math.sqrt(scala.math.Pi))

    // when
    val result1: Option[Double] = deserialize(serialize(doub1))
    val result2: Option[Double] = deserialize(serialize(doub2))
    val result3: Option[Double] = deserialize(serialize(doub3))
    val result4: Option[Double] = deserialize(serialize(doub4))

    // then
    result1.get should be(doub1)
    result2.get should be(doub2)
    result3.get should be(doub3)
    result4.get should be(doub4)
  }

  behavior of "Float"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = serialize(4567.567f)
    val bytes2: Array[Byte] = serialize(456f)
    val bytes3: Array[Byte] = serialize(-4567.567f)

    // when
    val result1: Option[Float] = deserialize(bytes1)
    val result2: Option[Float] = deserialize(bytes2)
    val result3: Option[Float] = deserialize(bytes3)

    // then
    result1.isDefined should be(true)
    result2.isDefined should be(true)
    result3.isDefined should be(true)
  }

  it should "be serializable" in {
    // given
    val float: Float = 3456.3456f

    // when
    serialize(float)

    // then
    true
  }

  it should "be reversible" in {
    // given
    val float1: Float = 4567.567f
    val float2: Float = 456f
    val float3: Float = -4567.567f
    val float4: Float =
      scala.math.pow(scala.math.E, scala.math.sqrt(scala.math.Pi)).toFloat

    // when
    val result1: Option[Float] = deserialize(serialize(float1))
    val result2: Option[Float] = deserialize(serialize(float2))
    val result3: Option[Float] = deserialize(serialize(float3))
    val result4: Option[Float] = deserialize(serialize(float4))

    // then
    result1.get should be(float1)
    result2.get should be(float2)
    result3.get should be(float3)
    result4.get should be(float4)
  }

  behavior of "Long"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = serialize(4567l)
    val bytes2: Array[Byte] = serialize(-456l)
    val bytes3: Array[Byte] = serialize(-4567.567f)

    // when
    val result1: Option[Long] = deserialize(bytes1)
    val result2: Option[Long] = deserialize(bytes2)
    val result3: Option[Long] = deserialize(bytes3)

    // then
    result1.isDefined should be(true)
    result2.isDefined should be(true)
    result3.isDefined should be(false)
  }

  it should "be serializable" in {
    // given
    val long: Long = 3456l

    // when
    serialize(long)

    // then
    true
  }

  it should "be reversible" in {
    // given
    val long1: Long = 4567l
    val long2: Long = -456l

    // when
    val result1: Option[Long] = deserialize(serialize(long1))
    val result2: Option[Long] = deserialize(serialize(long2))

    // then
    result1.get should be(long1)
    result2.get should be(long2)
  }

  behavior of "Unit"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = serialize((): Unit)
    val bytes2: Array[Byte] = serialize("")
    val bytes3: Array[Byte] = serialize("_")

    // when
    val result1: Option[Unit] = deserialize(bytes1)
    val result2: Option[Unit] = deserialize(bytes2)
    val result3: Option[Unit] = deserialize(bytes3)

    // then
    result1.isDefined should be(true)
    result2.isDefined should be(false)
    result3.isDefined should be(false)
  }

  it should "be serializable" in {
    // given
    val u1: Unit = Unit
    val u2: Unit = ()

    // when
    serialize(u1)
    serialize(u2)

    // then
    true
  }

  it should "be reversible" in {
    // given
    val u1: Unit = Unit
    val u2: Unit = ()

    // when
    val result1: Option[Unit] = deserialize(serialize(u1))
    val result2: Option[Unit] = deserialize(serialize(u2))

    // then
    result1.get should be(u1)
    result1.get should be(u2)
  }
}
