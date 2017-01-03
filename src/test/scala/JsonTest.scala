import org.scalatest.{FlatSpec, Matchers}

class JsonTest extends FlatSpec with Matchers {

  import argonaut._
  import com.cj.serialization._
  import com.cj.serialization.json._

  behavior of "SerializableJson"

  it should "serialize JSON constants correctly" in {
    // given: the software has been written

    // when
    val res1 = serialize(Json.jNull)
    val res2 = serialize(Json.jBool(false))
    val res3 = serialize(Json.jBool(true))

    // then
    res1 should be(serialize("null"))
    res2 should be(serialize("false"))
    res3 should be(serialize("true"))
  }

  it should "serialize JSON numbers correctly" in {
    // given
    val jnum = Json.jNumber(0l)

    // when
    val res = serialize(jnum)

    // then
    res should be(serialize("0"))
  }

  it should "serialize JSON strings correctly" in {
    // given
    val jstr = Json.jString("")

    // when
    val res = serialize(jstr)

    // then
    res should be(serialize(s""""""""))
  }

  it should "serialize an empty JSON array correctly" in {
    // given
    val arr = Json.array()

    // when
    val res = serialize(arr)

    // then
    res should be(serialize("[]"))
  }

  it should "serialize an empty JSON object correctly" in {
    // given
    val obj = Json.obj()

    // when
    val res = serialize(obj)

    // then
    res should be(serialize("{}"))
  }

  it should "serialize a populated `JsonArray` correctly" in {
    // given
    val arr = Json.array(Json.jNumber(0l), Json.jString(""))

    // when
    val res = serialize(arr)

    // then
    res should be(serialize("""[0,""]"""))
  }

  it should "serialize a populated `JsonObject` correctly" in {
    // given
    val obj = Json.obj(
      "value" -> Json.jNumber(0l),
      "added" -> Json.jString("")
    )
    val exp = serialize("""{"value":0,"added":""}""")

    // when
    val res = serialize(obj)

    // then
    res should be(exp)
  }

  // TODO: DeserializableJson tests
  behavior of "DeserializableJson"

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
