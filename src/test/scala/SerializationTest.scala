import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class SerializationTest extends FlatSpec with Matchers with PropertyChecks {

  import com.cj.serialization._

  "SerializationDemo" should "not be out of date" in {
    SerializationDemo.main(args = Array[String]())
  }

  behavior of "Serializable"

  it should "be open to post-hoc implementations" in {
    // given: a class
    case class Foo(bar: Int, baz: Char)
    val foo1 = Foo(123, 'a')
    val foo2 = Foo(-123, 'á')
    val expectedFooBytes1 = "Foo 123 a".getBytes
    val expectedFooBytes2 = "Foo -123 á".getBytes

    // when: we implement `Serializable[Foo]`
    implicit object SerializeFoo extends Serialize[Foo] {
      def serialize(t: Foo): Array[Byte] =
        s"""Foo ${t.bar} ${t.baz}""".getBytes
    }

    // then: `serialize` should work for arguments of type `Foo`
    serialize(foo1) sameElements expectedFooBytes1 should be(true)
    serialize(foo2) sameElements expectedFooBytes2 should be(true)
  }

  it should "be open to post-hoc implementations in general" in {
    // given: a class
    case class Foo(bar: Int, baz: Char)

    // when: we implement `Serializable[Foo]`
    implicit object SerializableFoo extends Serialize[Foo] {
      def serialize(t: Foo): Array[Byte] =
        s"""Foo ${t.bar} ${t.baz}""".getBytes
    }

    // then: `serialize` should work for arguments of type `Foo`
    forAll { (n: Int, c: Char) =>
      val foo = Foo(n, c)
      serialize(foo) sameElements s"Foo $n $c".getBytes should be(true)
    }
  }

  behavior of "Deserializable"

  it should "be open to post-hoc implementations" in {
    // given: a class
    case class Foo(bar: Int, baz: Char)
    val fooBytes1 = "Foo 123 a".getBytes
    val fooBytes2 = "Foo -123 á".getBytes
    val expectedFoo1 = Foo(123, 'a')
    val expectedFoo2 = Foo(-123, 'á')

    // when: we implement `Deserialize[Foo]`
    implicit object DeserializeFoo extends Deserialize[Foo] {

      private val regex = """Foo (-?\d+) (.)""".r

      def deserialize(bytes: Array[Byte]): Result[Foo] = {
        new String(bytes) match {
          case regex(nStr, cStr) => for {
            n <- Result.safely(nStr.toInt)
            c <- Result.safely(cStr.head)
          } yield Foo(n, c)
          case _ => Result.failure(s"Failed to deserialize $bytes to Foo")
        }
      }
    }

    // then: `deserialize` should be able to return values of type `Option[Foo]`
    deserialize[Foo](fooBytes1).contains(expectedFoo1) should be(true)
    deserialize[Foo](fooBytes2).contains(expectedFoo2) should be(true)
  }

  it should "be open to post-hoc implementations in general" in {
    // given: a class
    case class Foo(bar: Int, baz: Char)

    // when: we implement `Deserialize[Foo]`
    implicit object DeserializableFoo extends Deserialize[Foo] {

      private val regex = """Foo (-?\d+) (.)""".r

      def deserialize(bytes: Array[Byte]): Result[Foo] = {
        new String(bytes) match {
          case regex(nStr, cStr) => for {
            n <- Result.safely(nStr.toInt)
            c <- Result.safely(cStr.head)
          } yield Foo(n, c)
          case _ => Result.failure(s"Failed to deserialize $bytes to Foo")
        }
      }
    }

    // then
    forAll { (n: Int, c: Char) =>
      val fooBytes: Array[Byte] = s"Foo $n $c".getBytes
      deserialize[Foo](fooBytes).contains(Foo(n, c)) should be(true)
    }
  }

  behavior of "String"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = "foó".getBytes
    val bytes2: Array[Byte] = "".getBytes

    // when
    val result1: Result[String] = deserialize(bytes1)
    val result2: Result[String] = deserialize(bytes2)

    // then
    result1.isSuccess should be(true)
    result2.isSuccess should be(true)
  }

  it should "be deserializable in general" in {
    // given

    // when

    // then
    forAll { (s: String) =>
      deserialize[String](s.getBytes).isSuccess should be(true)
    }
  }

  it should "be serializable" in {
    // given
    val string: String = "foo"

    // when
    val result = Result.safely(serialize(string))

    // then
    result.isSuccess should be(true)
  }

  it should "be serializable in general" in {
    // given

    // when

    // then
    forAll { (s: String) =>
      // we're checking that it didn't throw and that it didn't return null
      Result.safely(serialize(s)).isSuccess should be(true)
    }
  }

  it should "be reversible" in {
    // given
    val string1: String = "foo"
    val string2: String = ""

    // when
    val result1: Result[String] = deserialize(serialize(string1))
    val result2: Result[String] = deserialize(serialize(string2))

    // then
    result1.contains(string1) should be(true)
    result2.contains(string2) should be(true)
  }

  it should "be reversible in general" in {
    // given

    // when

    // then
    forAll { (s: String) =>
      val left: Result[String] = deserialize(serialize(s))
      val right: Result[String] = Result.safely(s)
      left == right should be(true)
    }

    // and
    forAll { (bytes: Array[Byte]) =>
      val left: Result[String] =
        deserialize[String](bytes)
          .map(serialize[String])
          .flatMap(deserialize[String])
      val right: Result[String] = deserialize[String](bytes)
      left == right should be(true)
    }
  }

  it should "handle non-ascii characters" in {
    // given
    val stringWithCharacter: String =
      """{
        |  "prénom" : "Brešar",
        |  "mère" : "ßoë"
        |}
      """.stripMargin

    // when
    val result: Result[String] = deserialize(serialize(stringWithCharacter))

    // then
    result.contains(stringWithCharacter) should be(true)
  }

  it should "never throw or return a null when deserializing" in {
    // given

    // when

    // then
    forAll { (bytes: Array[Byte]) =>
      Result.safely(deserialize[String](bytes)).isSuccess should be(true)
      deserialize[String](bytes).contains(null) should be(false)
    }
  }

  it should "never throw or return a null when serializing" in {
    // given

    // when

    // then
    forAll { (s: String) =>
      Result.safely(serialize(s)).isSuccess should be(true)
      serialize(s) == null should be(false)
    }
  }

  behavior of "Boolean"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = "foo".getBytes
    val bytes2: Array[Byte] = "false".getBytes
    val bytes3: Array[Byte] = "true".getBytes

    // when
    val result1: Result[Boolean] = deserialize(bytes1)
    val result2: Result[Boolean] = deserialize(bytes2)
    val result3: Result[Boolean] = deserialize(bytes3)

    // then
    result1.isSuccess should be(false)
    result2.isSuccess should be(true)
    result3.isSuccess should be(true)
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

  it should "never throw or return a null when deserializing" in {
    // given

    // when

    // then
    forAll { (bytes: Array[Byte]) =>
      Result.safely(deserialize[Boolean](bytes)).isSuccess should be(true)
      deserialize[Boolean](bytes).contains(null) should be(false)
    }
  }

  it should "never throw or return a null when serializing" in {
    // given

    // when

    // then
    forAll { (p: Boolean) =>
      Result.safely(serialize(p)).isSuccess should be(true)
      serialize(p) == null should be(false)
    }
  }

  it should "be reversible" in {
    // given
    val bool1: Boolean = false
    val bool2: Boolean = true

    // when
    val result1: Result[Boolean] = deserialize(serialize(bool1))
    val result2: Result[Boolean] = deserialize(serialize(bool2))

    // then
    result1.getOrThrow should be(bool1)
    result2.getOrThrow should be(bool2)
  }

  it should "be reversible in general" in {
    // given

    // when

    //then
    forAll { (p: Boolean) =>
      val left = deserialize[Boolean](serialize(p))
      val right = Result.safely(p)
      left == right should be(true)
    }

    // and
    forAll { (bs: Array[Byte]) =>
      val left =
        deserialize[Boolean](bs)
          .map(serialize[Boolean])
          .flatMap(deserialize[Boolean])
      val right = deserialize[Boolean](bs)
      left == right should be(true)
    }
  }

  behavior of "Byte"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = "foo".getBytes
    val bytes2: Array[Byte] = "f".getBytes
    val bytes3: Array[Byte] = "".getBytes

    // when
    val result1: Result[Byte] = deserialize(bytes1)
    val result2: Result[Byte] = deserialize(bytes2)
    val result3: Result[Byte] = deserialize(bytes3)

    // then
    result1.isSuccess should be(false)
    result2.isSuccess should be(true)
    result3.isSuccess should be(false)
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
    val result: Result[Byte] = deserialize(serialize(byte))

    // then
    result.getOrThrow should be(byte)
  }

  it should "be reversible in general" in {
    // given

    // when

    // then
    forAll { (b: Byte) =>
      val left = deserialize[Byte](serialize(b))
      val right = Result.safely(b)
      left == right should be(true)
    }

    // and
    forAll { (bs: Array[Byte]) =>
      val left = deserialize[Byte](bs)
        .map(serialize[Byte])
        .flatMap(deserialize[Byte])
      val right = deserialize[Byte](bs)
      left == right should be(true)
    }
  }

  it should "never throw or return null when deserializing" in {
    // given

    // when

    // then
    forAll { (bytes: Array[Byte]) =>
      Result.safely(deserialize[Byte](bytes)).isSuccess should be(true)
      deserialize[Byte](bytes).contains(null) should be(false)
    }
  }

  it should "never throw or return null when serializing" in {
    // given

    // when

    // then
    forAll { (b: Byte) =>
      Result.safely(serialize(b)).isSuccess should be(true)
      serialize(b) == null should be(false)
    }
  }

  behavior of "Char"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = "foo".getBytes
    val bytes2: Array[Byte] = "f".getBytes
    val bytes3: Array[Byte] = "".getBytes

    // when
    val result1: Result[Char] = deserialize(bytes1)
    val result2: Result[Char] = deserialize(bytes2)
    val result3: Result[Char] = deserialize(bytes3)

    // then
    result1.isSuccess should be(false)
    result2.isSuccess should be(true)
    result3.isSuccess should be(false)
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
    val result: Result[Char] = deserialize(serialize(char))

    // then
    result.getOrThrow should be(char)
  }

  it should "be reversible in general" in {
    forAll { (c: Char) =>
      val left = deserialize[Char](serialize(c))
      val right = Result.safely(c)
      left == right should be(true)
    }
    forAll { (bs: Array[Byte]) =>
      val left = deserialize[Char](bs)
        .map(serialize[Char])
        .flatMap(deserialize[Char])
      val right = deserialize[Char](bs)
      left == right should be(true)
    }
  }

  it should "never throw or return null when deserializing" in {
    forAll { (bs: Array[Byte]) =>
      Result.safely(deserialize[Char](bs)).isSuccess should be(true)
      deserialize[Char](bs).contains(null) should be(false)
    }
  }

  it should "never throw or return null when serializing" in {
    forAll { (c: Char) =>
      Result.safely(serialize(c)).isSuccess should be(true)
      serialize(c) == null should be(false)
    }
  }

  behavior of "Double"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = serialize(4567.567)
    val bytes2: Array[Byte] = serialize(456)
    val bytes3: Array[Byte] = serialize(-4567.567)

    // when
    val result1: Result[Double] = deserialize(bytes1)
    val result2: Result[Double] = deserialize(bytes2)
    val result3: Result[Double] = deserialize(bytes3)

    // then
    result1.isSuccess should be(true)
    result2.isSuccess should be(true)
    result3.isSuccess should be(true)
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
    val result1: Result[Double] = deserialize(serialize(doub1))
    val result2: Result[Double] = deserialize(serialize(doub2))
    val result3: Result[Double] = deserialize(serialize(doub3))
    val result4: Result[Double] = deserialize(serialize(doub4))

    // then
    result1.getOrThrow should be(doub1)
    result2.getOrThrow should be(doub2)
    result3.getOrThrow should be(doub3)
    result4.getOrThrow should be(doub4)
  }

  it should "never throw or return null when deserializing" in {
    forAll { (bs: Array[Byte]) =>
      Result.safely(deserialize[Double](bs)).isSuccess should be(true)
      deserialize[Double](bs).contains(null) should be(false)
    }
  }

  it should "never throw or return null when serializing" in {
    forAll { (x: Double) =>
      Result.safely(serialize(x)).isSuccess should be(true)
      serialize(x) == null should be(false)
    }
  }

  it should "be reversible in general" in {
    forAll { (x: Double) =>
      val left = deserialize[Double](serialize(x))
      val right = Result.safely(x)
      left == right should be(true)
    }
    forAll { (bs: Array[Byte]) =>
      val left = deserialize[Double](bs)
        .map(serialize[Double])
        .flatMap(deserialize[Double])
      val right = deserialize[Double](bs)
      left == right should be(true)
    }
  }

  behavior of "Float"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = serialize(4567.567f)
    val bytes2: Array[Byte] = serialize(456f)
    val bytes3: Array[Byte] = serialize(-4567.567f)

    // when
    val result1: Result[Float] = deserialize(bytes1)
    val result2: Result[Float] = deserialize(bytes2)
    val result3: Result[Float] = deserialize(bytes3)

    // then
    result1.isSuccess should be(true)
    result2.isSuccess should be(true)
    result3.isSuccess should be(true)
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
    val result1: Result[Float] = deserialize(serialize(float1))
    val result2: Result[Float] = deserialize(serialize(float2))
    val result3: Result[Float] = deserialize(serialize(float3))
    val result4: Result[Float] = deserialize(serialize(float4))

    // then
    result1.getOrThrow should be(float1)
    result2.getOrThrow should be(float2)
    result3.getOrThrow should be(float3)
    result4.getOrThrow should be(float4)
  }

  behavior of "Long"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = serialize(4567l)
    val bytes2: Array[Byte] = serialize(-456l)
    val bytes3: Array[Byte] = serialize(-4567.567f)

    // when
    val result1: Result[Long] = deserialize(bytes1)
    val result2: Result[Long] = deserialize(bytes2)
    val result3: Result[Long] = deserialize(bytes3)

    // then
    result1.isSuccess should be(true)
    result2.isSuccess should be(true)
    result3.isSuccess should be(false)
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
    val result1: Result[Long] = deserialize(serialize(long1))
    val result2: Result[Long] = deserialize(serialize(long2))

    // then
    result1.getOrThrow should be(long1)
    result2.getOrThrow should be(long2)
  }

  behavior of "Unit"

  it should "be deserializable" in {
    // given
    val bytes1: Array[Byte] = serialize((): Unit)
    val bytes2: Array[Byte] = serialize("")
    val bytes3: Array[Byte] = serialize("_")

    // when
    val result1: Result[Unit] = deserialize(bytes1)
    val result2: Result[Unit] = deserialize(bytes2)
    val result3: Result[Unit] = deserialize(bytes3)

    // then
    result1.isSuccess should be(true)
    result2.isSuccess should be(false)
    result3.isSuccess should be(false)
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
    val result1: Result[Unit] = deserialize(serialize(u1))
    val result2: Result[Unit] = deserialize(serialize(u2))

    // then
    result1.getOrThrow should be(u1)
    result2.getOrThrow should be(u2)
  }
}
