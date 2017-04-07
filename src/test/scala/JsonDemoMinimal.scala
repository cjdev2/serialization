object JsonDemoMinimal extends App {

  // It's come to my attention that some don't like polluting their global scope
  // with all kinds of implicits. More to the point, if you create an implicit
  // `JsonSerializer` for `Person` and another implicit `Serializable[Person]`
  // (say for Thrift), then `serialize` becomes undecidable, even when type-
  // annotated (a compile-time error at least, but a frustrating one if you
  // don't know what's going on behind the scenes.
  //
  // If you are running into the above problem, one solution is to avoid using
  // underscore, "_", in your import statements and to declare your serializers
  // explicit instead of implicit. This short demo illustrates those practices.

  import com.cj.serialization.json.{CodecJson, JsonSerializer, jsonSerializerFromCodec}
  import com.cj.serialization.thrift.SerializableThriftStruct
  import local.test.serialization.thrift.scala.TestRecord

  val ThriftS = SerializableThriftStruct

  // Make a codec. The easiest way is to use Argonaut's "caseCodec" functions
  val testRecordCodec: CodecJson[TestRecord] =
    argonaut.Argonaut.casecodec2[String, Long, TestRecord](
      TestRecord.apply,
      testRecord => TestRecord.unapply(testRecord).map(p => (p._1, p._2))
    )("foo", "bar")

  // Make a JsonSerializer.
  val testRecordJsonSerializer: JsonSerializer[TestRecord] =
    jsonSerializerFromCodec(testRecordCodec)

  // Use the serializer on your record
  val record = TestRecord("Tim Drake", 19)
  assert(
    testRecordJsonSerializer.toPrettyJsonString(record) ==
      """{
        |  "foo" : "Tim Drake",
        |  "bar" : 19
        |}""".stripMargin
  )
  assert(
    ThriftS.serialize(record).mkString(",") ==
      "11,0,1,0,0,0,9,84,105,109,32,68,114,97,107,101,10,0,2,0,0,0,0,0,0,0,19,0,0,0,0,0"
  )

  val batmanString =
    """{"foo":"Batman","bar":38}"""
  assert(
    testRecordJsonSerializer.fromJsonString(batmanString).contains(
      TestRecord("Batman", 38)
    )
  )
  assert(
    testRecordJsonSerializer.fromJsonString(batmanString)
      .map(ThriftS.serialize)
      .get.mkString(",")
    == "11,0,1,0,0,0,6,66,97,116,109,97,110,10,0,2,0,0,0,0,0,0,0,38,0,0,0,0,0,0,0,0"
  )
}
