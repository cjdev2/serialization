import org.scalatest.{FlatSpec, Matchers}

class JsonTest extends FlatSpec with Matchers {

  import com.cj.serialization._
  import com.cj.serialization.json._

  behavior of "SerializableJsonValue"

  it should "serialize `JsonConstant`s correctly" in {
    // given: the software has been written

    // when
    val res1 = serialize(JsonNull)
    val res2 = serialize(JsonFalse)
    val res3 = serialize(JsonTrue)

    // then
    res1 should be(serialize("null"))
    res2 should be(serialize("false"))
    res3 should be(serialize("true"))
  }

  it should "serialize `JsonNumber`s correctly" in {
    // given
    val jnum = JsonNumber(0.toDouble)

    // when
    val res = serialize(jnum)

    // then
    res should be(serialize("0"))
  }

  it should "serialize `JsonString`s correctly" in {
    // given
    val jstr = JsonString("")

    // when
    val res = serialize(jstr)

    // then
    res should be(serialize(s""""""""))
  }

  it should "serialize an empty `JsonArray` correctly" in {
    // given
    val arr = JsonArray(Seq())

    // when
    val res = serialize(arr)

    // then
    res should be(serialize("[]"))
  }

  it should "serialize an empty `JsonObject` correctly" in {
    // given
    val obj = JsonObject(Map())

    // when
    val res = serialize(obj)

    // then
    res should be(serialize("{}"))
  }

  it should "serialize a populated `JsonArray` correctly" in {
    // given
    val arr = JsonArray(Seq(JsonNumber(0.toDouble), JsonString("")))

    // when
    val res = serialize(arr)

    // then
    res should be(serialize("""[0,""]"""))
  }

  it should "serialize a populated `JsonObject` correctly" in {
    // given
    val obj = JsonObject(Map(
      "value" -> JsonNumber(0.toDouble),
      "added" -> JsonString("")
    ))
    val exp = serialize("""{"value":0,"added":""}""")

    // when
    val res = serialize(obj)

    // then
    res should be(exp)
  }

  // TODO: DeserializableJsonValue tests
  behavior of "DeserializableJsonValue"

  it should "do the thing" in {
    fail
  }

  // TODO: test round trip, test concurrency
  behavior of "JsonValue"

  it should "be nice" in {
    fail
  }

  // TODO: JsonSerializer tests
  behavior of "JsonSerializer"

  it should "do good" in {
    fail
  }

  // TODO: JsonDeserializer tests
  behavior of "JsonDeserializer"

  it should "done well" in {
    fail
  }

  // TODO: test round trip, test concurrency
  behavior of "serialization contract"

  it should "do everything I say it does" in {
    fail
  }
}
