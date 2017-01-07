object SerializationDemo extends App {

  import com.cj.serialization._

  case class Foo(bar: Int)

  implicit object FooSerializer extends Serializable[Foo] {
    def serialize(t: Foo): Array[Byte] =
      t.toString.getBytes
  }

  implicit object FooDeserializer extends Deserializable[Foo] {
    def deserialize(bytes: Array[Byte]): Option[Foo] = {
      val string: String = new String(bytes)
      val regex = "Foo\\((\\d+)\\)".r

      string match {
        case regex(int) => Option(Foo(int.toInt))
        case _ => None
      }
    }
  }

  val fooVal: Foo = Foo(1234)
  serialize(fooVal) // returns a value of type `Array[Byte]`

  val fooBytes: Array[Byte] = "Person(5678)".getBytes
  deserialize[Foo](fooBytes) // returns a value of type `Option[Foo]`

  assert(
    // `fooVal` survives serialization followed by deserialization
    deserialize[Foo](serialize(fooVal)).contains(fooVal)
  )
  assert(
    // `fooBytes` survives deserialization followed by serialization
    deserialize[Foo](fooBytes)
      .map(serialize[Foo])
      .flatMap(deserialize[Foo])
      == deserialize[Foo](fooBytes)
    // we deserialize, then serialize, then deserialize, and the result should
    // be the same as when we simply deserialize once (modulo the effects of
    // `Option`). So, ignoring the `Option` we are checking that
    // `des(ser(des(bytes))) == des(bytes).
    //
    // Q: Why don't we simply check that deserializing followed by serializing
    //    returns the original byte array, i.e. that ser(des(bytes)) == bytes?
    // A: Because different byte arrays might describe the same data!
    //
    // Example:
    //   Say we have `case class Foo(bar: Int, baz: Int)` and say that we have
    //   defined `serialize` so that `serialize(Foo(x,y))` gives
    //   `{"bar":x,"baz":y}`.
    //
    //   Now, `{"bar":0,"baz":0}` and `{ "bar" : 0, "baz" : 0 }` both
    //   deserialize to `Foo(0,0)` (if our parser is not stupid), which in turn
    //   gets serialized to `{"bar":0,"baz":0}`, not to
    //   `{ "bar" : 0, "baz" : 0 }`, so asserting that deserializing followed by
    //   serializing should return the original byte array will fail.
    //
    // Essentially, we consider two JSON strings to be the same if they encode
    // the same data, so instead of checking that ser(des(bytes)) == bytes, we
    // decode both sides to get at the actual data, thus we check
    // des(ser(des(bytes)) == des(bytes).
  )
}
