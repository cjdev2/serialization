import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class JsonTest extends FlatSpec with Matchers with PropertyChecks {

  import argonaut.{Argonaut, Json}
  import com.cj.serialization._
  import com.cj.serialization.json._
  import Fixtures._

  "JsonDemo" should "not be out of date" in {
    JsonDemo.main(args = Array[String]())
  }

  "JsonDemoMinimal" should "not be out of date" in {
    JsonDemoMinimal.main(args = Array[String]())
  }

  behavior of "JsonSerializerJson"

  it should "serialize JSON constants correctly" in {
    // given

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

  it should "serialize an empty JSON strings correctly" in {
    // given
    val jstr = Json.jString("")

    // when
    val res = serialize(jstr)

    // then
    res should be(serialize(s""""""""))
  }

  it should "serialize an arbitrary (lol!) JSON strings correctly" in {
    // given
    val jstr = Json.jString("arbitrary")

    // when
    val res = serialize(jstr)

    // then
    res should be(serialize(s""""arbitrary""""))
  }

  it should "serialize JSON strings with wonky characters" in {
    // given
    val jstr = Json.jString("Pokémon, Go!")

    // when
    val res = serialize(jstr)

    // then
    res should be(serialize(s""""Pokémon, Go!""""))
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
    val obj = Json()

    // when
    val res = serialize(obj)

    // then
    res should be(serialize("{}"))
  }

  it should "serialize a populated JSON array correctly" in {
    // given
    val arr = Json.array(Json.jNumber(0l), Json.jString(""))

    // when
    val res = serialize(arr)

    // then
    res should be(serialize("""[0,""]"""))
  }

  it should "serialize a populated JSON object correctly" in {
    // given
    val obj = Json(
      "value" -> Json.jNumber(0l),
      "added" -> Json.jString("")
    )
    val exp = serialize("""{"value":0,"added":""}""")

    // when
    val res = serialize(obj)

    // then
    res should be(exp)
  }

  it should "satisfy its contract" in {
    // given
    val jsonTestCases = Seq[Json](
      jsonTim,
      jsonBruce,
      jsonBatman,
      jsonNested,
      jsonWithCharacter,
      jsonBruceFrench
    )
    val stringTestCases = Seq[String](
      stringTim,
      stringBruce,
      stringBatman,
      stringNested,
      stringWithCharacter,
      stringBruceFrench
    )
    val bytesTestCases = Seq[Array[Byte]](
      bytesTim,
      bytesBruce,
      bytesBatman,
      bytesNested,
      bytesWithCharacter,
      bytesBruceFrench
    )

    // when: a JsonSerializer[Json] is in scope
    // included by `import com.cj.serialization.json._`

    // then
    jsonTestCases.foreach(leftInverseTestCase[Json])
    jsonTestCases.foreach(jsonRightPseudoInverseTestCase[Json])
    stringTestCases.foreach(stringRightPseudoInverseTestCase[Json])
    bytesTestCases.foreach(bytesRightPseudoInverseTestCase[Json])
  }

  behavior of "JsonSerializerFromCodec"

  it should "satisfy its contract when `T = Person`" in {
    // give
    val personTestCases = Seq[Person](
      personTim,
      personBruce,
      personBatman
    )
    val jsonTestCases = Seq[Json](
      jsonTim,
      jsonBruce,
      jsonBatman
    )
    val stringTestCases = Seq[String](
      stringTim,
      stringBruce,
      stringBatman
    )
    val bytesTestCases = Seq[Array[Byte]](
      bytesTim,
      bytesBruce,
      bytesBatman
    )

    // when: JsonSerializerFromCodec creates a JsonSerializer[Pair]
    implicit val personCodec: CodecJson[Person] =
      Argonaut.casecodec4(Person.apply, Person.unapply)("name", "age", "things", "mother")

    // then
    personTestCases.foreach(leftInverseTestCase[Person])
    jsonTestCases.foreach(jsonRightPseudoInverseTestCase[Person])
    stringTestCases.foreach(stringRightPseudoInverseTestCase[Person])
    bytesTestCases.foreach(bytesRightPseudoInverseTestCase[Person])
  }

  it should "satisfy its contract when `T = Pair`" in {
    // given
    val pairTestCases = Seq[Pair](
      pair
    )
    val jsonTestCases = Seq[Json](
      jsonPair1
    )
    val stringTestCases = Seq[String](
      stringPair1
    )
    val bytesTestCases = Seq[Array[Byte]](
      bytesPair1
    )

    // when: JsonSerializerFromCodec creates a JsonSerializer[Pair]
    implicit val pairCodec: CodecJson[Pair] = {

      implicit def keyCodec =
        Argonaut.casecodec1(Key.apply, Key.unapply)("get")

      implicit def valueCodec =
        Argonaut.casecodec1(Value.apply, Value.unapply)("get")

      Argonaut.casecodec2(Pair.apply, Pair.unapply)("key", "value")
    }

    // then
    pairTestCases.foreach(leftInverseTestCase[Pair])
    jsonTestCases.foreach(jsonRightPseudoInverseTestCase[Pair])
    stringTestCases.foreach(stringRightPseudoInverseTestCase[Pair])
    bytesTestCases.foreach(bytesRightPseudoInverseTestCase[Pair])
  }

  it should "Just Work™ with Scala Lists" in {
    // given: some JSON with some known shape
    val input =
      """
      [
        { "name": "Mark", "age": 191 },
        { "name": "Fred", "age": 33, "greeting": "hey ho, lets go!" },
        { "name": "Barney", "age": 35, "address": {
          "street": "rock street", "number": 10, "post_code": 2039
        }}
      ]
      """

    // when: you make structs for the objects and make instances of
    // JsonSerializerFromCodec for those structs
    case class Address(
                        street: String,
                        number: Int,
                        postcode: Int
                      )

    implicit val addressCodec: argonaut.CodecJson[Address] =
      argonaut.Argonaut.casecodec3(Address.apply, Address.unapply)(
        "street", "number", "post_code"
      )

    case class Person(
                       name: String,
                       age: Int,
                       address: Option[Address],
                       greeting: Option[String]
                     )

    implicit val personCodec: CodecJson[Person] =
      argonaut.Argonaut.casecodec4(Person.apply, Person.unapply)(
        "name", "age", "address", "greeting"
      )

    // then: you can deserialize and work with JSON arrays of those
    // structs without creating another JsonSerializer specifically
    // for JSON arrays
    val people = fromJsonString[List[Person]](input).getOrElse(Nil)
    val nice = people.map(person =>
      person.copy(greeting = person.greeting.orElse(Some("Hello good sir!")))
    )
    val output = toPrettyJsonString(nice)
    assert(
      output ==
        """[
          |  {
          |    "name" : "Mark",
          |    "age" : 191,
          |    "address" : null,
          |    "greeting" : "Hello good sir!"
          |  },
          |  {
          |    "name" : "Fred",
          |    "age" : 33,
          |    "address" : null,
          |    "greeting" : "hey ho, lets go!"
          |  },
          |  {
          |    "name" : "Barney",
          |    "age" : 35,
          |    "address" : {
          |      "street" : "rock street",
          |      "number" : 10,
          |      "post_code" : 2039
          |    },
          |    "greeting" : "Hello good sir!"
          |  }
          |]""".stripMargin
    )
  }

  it should "satisfy the contract for general arguments" in {
    // given
    implicit val personCodec: CodecJson[Person] =
      Argonaut.casecodec4(Person.apply, Person.unapply)(
        "name", "age", "things", "mother"
      )

    forAll { (n: String, a: Int, ts: List[String], mo: Option[String]) =>

      val person = Person(n, a, ts, mo)

      // when/then
      deserialize[Person](serialize(person)) should be(Some(person))
    }
  }

  it should "be thread safe" in {
    // given
    implicit val personCodec: CodecJson[Person] =
      Argonaut.casecodec4(Person.apply, Person.unapply)(
        "name", "age", "things", "mother"
      )

    forAll { (ps: List[(String, Int, List[String], Option[String])]) =>

      whenever (ps.nonEmpty) {

        val people = ps.map(Person.tupled)

        // when
        val strictResult = people.map(serialize[Person])
        val concurrentResult = people.par.map(serialize[Person])

        // then
        strictResult
          .zip(concurrentResult)
          .map({ case (z1, z2) => z1 sameElements z2 })
          .reduce(_ && _) should be(true)
      }
    }
  }

  behavior of "JsonSerializerFromConverters"

  it should "satisfy its contract" in {
    // given
    val pairTestCases = Seq[Pair](
      pair
    )
    val jsonTestCases = Seq[Json](
      jsonPair2
    )
    val stringTestCases = Seq[String](
      stringPair2
    )
    val bytesTestCases = Seq[Array[Byte]](
      bytesPair2
    )

    // when: JsonSerializerFromConverters creates a JsonSerializer[Pair]
    implicit object PairSerializer
      extends JsonSerializerFromConverters[Pair](
        to = pairToJson,
        from = jsonToPair
      )

    // then
    pairTestCases.foreach(leftInverseTestCase[Pair])
    jsonTestCases.foreach(jsonRightPseudoInverseTestCase[Pair])
    stringTestCases.foreach(stringRightPseudoInverseTestCase[Pair])
    bytesTestCases.foreach(bytesRightPseudoInverseTestCase[Pair])
  }

  object Fixtures {

    case class Key(get: Int)
    case class Value(get: String)
    case class Pair(key: Key, value: Value)

    val pair: Pair = Pair(Key(5),Value("foo"))

    val jsonPair1: Json = Json(
      "key" -> Json("get" -> Json.jNumber(5)),
      "value" -> Json("get" -> Json.jString("foo"))
    )

    val stringPair1: String =
      s"""{
         |  "key" : { "get" : 5 },
         |  "value" : { "get" : "foo }
         |}""".stripMargin

    val bytesPair1: Array[Byte] = stringPair1.getBytes("UTF-8")

    def pairToJson(pair: Pair): Json = Json(
      "key" -> Json.jNumber(pair.key.get),
      "value" -> Json.jString(pair.value.get)
    )

    def jsonToPair(json: Json): Option[Pair] = for {
      propList <- json.assoc
      keyJson <- propList.toMap.get("key")
      keyJNum <- keyJson.number
      key <- keyJNum.toInt
      valueJson <- propList.toMap.get("value")
      value <- valueJson.string
    } yield Pair(Key(key), Value(value))

    val jsonPair2: Json = Json(
      "key" -> Json.jNumber(5),
      "value" -> Json.jString("foo")
    )

    val stringPair2: String = """{"key":5,"value":"foo"}"""

    val bytesPair2: Array[Byte] = stringPair2.getBytes("UTF-8")

    case class Person(
                       name: String,
                       age: Int,
                       things: List[String],
                       mother: Option[String]
                     )

    val personTim: Person =
      Person("Tim Drake",19,List("Bo"),Some("Janet Drake"))

    val personBruce: Person =
      Person("Bruce Wayne",38,List("Money", "Alfred"),Some("Martha Wayne"))

    val personBatman: Person =
      Person("Batman",38,List("Batarang", "Batmobile"),None)

    val jsonTim: Json = Json(
      "name" -> Json.jString("Tim Drake"),
      "age" -> Json.jNumber(19),
      "things" -> Json.array(
        Json.jString("Bo")
      ),
      "mother" -> Json.jString("Janet Drake")
    )

    val jsonBruce: Json = Json(
      "name" -> Json.jString("Bruce Wayne"),
      "age" -> Json.jNumber(38),
      "things" -> Json.array(
        Json.jString("Money"),
        Json.jString("Alfred")
      ),
      "mother" -> Json.jString("Martha Wayne")
    )

    val jsonBatman: Json = Json(
      "name" -> Json.jString("Batman"),
      "age" -> Json.jNumber(38),
      "things" -> Json.array(
        Json.jString("Batarang"),
        Json.jString("Batmobile")
      )
    )

    val jsonNested: Json = Json(
      "this" -> Json.jString("that"),
      "those" -> Json.array(
        Json.jString("these"),
        Json.jString("others")
      ),
      "me" -> jsonTim,
      "them" -> Json.array(
        jsonBruce,
        jsonBatman
      )
    )

    val jsonWithCharacter: Json = Json(
      "name" -> Json.jString("Daniel"),
      "favorite_game" -> Json.jString("¡Pokémon Snap!")
    )

    val jsonBruceFrench: Json = Json(
      "prénome" -> Json.jString("Bruce Wayne"),
      "âge" -> Json.jNumber(38),
      "des_choses" -> Json.array(
        Json.jString("Le Argent"),
        Json.jString("Alfred")
      ),
      "mère" -> Json.jString("Martha Wayne")
    )

    val stringTim: String =
      """{"name":"Tim Drake","age":19,"things":["Bo"],"mother":"Janet Drake"}"""

    val stringBruce: String =
      """{
        |  "name" : "Bruce Wayne",
        |  "age" : 38,
        |  "things" : [
        |    "Money",
        |    "Alfred"
        |  ],
        |  "mother" : "Martha Wayne"
        |}""".stripMargin

    val stringBatman: String =
      """{"name":"Batman","age":38,"things":["Batarang","Batmobile"]}"""

    val stringNested: String =
      """{
         |  "this" : "that",
         |  "those" : [
         |    "these",
         |    "others"
         |  ],
         |  "me" : {
         |    "name" : "Tim Drake",
         |    "age" : 19,
         |    "things" : [
         |      "Bo"
         |    ],
         |    "mother" : "Janet Drake"
         |  },
         |  "them" : [
         |    {
         |      "name" : "Bruce Wayne",
         |      "age" : 38,
         |      "things" : [
         |        "Money",
         |        "Alfred"
         |      ],
         |      "mother" : "Martha Wayne"
         |    },
         |    {
         |      "name" : "Batman",
         |      "age" : 38,
         |      "things" : [
         |        "Batarang",
         |        "Batmobile"
         |      ]
         |    }
         |  ]
         |}""".stripMargin

    val stringWithCharacter: String =
      """{
        |  "name" : "Daniel",
        |  "favorite_game" : "¡Pokémon Snap!"
        |}
      """.stripMargin

    val stringBruceFrench: String =
      """{
        |  "prénome" : "Bruce Wayne",
        |  "âge" : 38,
        |  "des_choses" : [
        |    "Le Argent:,
        |    "Alfred"
        |  ],
        |  "mère" : "Martha Wayne"
        |}""".stripMargin

    val bytesTim: Array[Byte] = stringTim.getBytes("UTF-8")

    val bytesBruce: Array[Byte] = stringBruce.getBytes("UTF-8")

    val bytesBatman: Array[Byte] = stringBatman.getBytes("UTF-8")

    val bytesNested: Array[Byte] = stringNested.getBytes("UTF-8")

    val bytesWithCharacter: Array[Byte] = stringWithCharacter.getBytes("UTF-8")

    val bytesBruceFrench: Array[Byte] = stringBruceFrench.getBytes("UTF-8")

    def leftInverseTestCase[T: JsonSerializer](t: T): Unit = {
      assert(
        fromJson[T](toJson[T](t)).contains(t)
      )
      assert(
        fromJsonString[T](toJsonString[T](t)).contains(t)
      )
      assert(
        fromJsonString[T](toPrettyJsonString[T](t)).contains(t)
      )
      assert(
        deserialize[T](serialize[T](t)).contains(t)
      )
    }

    def jsonRightPseudoInverseTestCase[T: JsonSerializer](json: Json)
    : Unit = {
      assert(
        fromJson[T](json)
          .map(toJson[T])
          .flatMap(fromJson[T])
          == fromJson[T](json)
      )
    }

    def stringRightPseudoInverseTestCase[T: JsonSerializer](string: String)
    : Unit = {
      assert(
        fromJsonString[T](string)
          .map(toJsonString[T])
          .flatMap(fromJsonString[T])
          == fromJsonString[T](string)
      )
      assert(
        fromJsonString[T](string)
          .map(toPrettyJsonString[T])
          .flatMap(fromJsonString[T])
          == fromJsonString[T](string)
      )
    }

    def bytesRightPseudoInverseTestCase[T: JsonSerializer](bytes: Array[Byte])
    : Unit = {
      assert(
        deserialize[T](bytes)
          .map(serialize[T])
          .flatMap(deserialize[T])
          == deserialize[T](bytes)
      )
    }
  }
}
