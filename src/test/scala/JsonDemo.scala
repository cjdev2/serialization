object JsonDemo extends App {

  // TODO: Table of Contents
  //
  // Case Classes and Automatic Codec Generation
  // Basic Usage
  // Validation Guarantees
  // Algebraic Data Types
  // Nested Case Classes
  // Multiple Serializers in Scope
  // Case Classes with Type Parameters

  import com.cj.serialization._
  import com.cj.serialization.json._, JsonImplicits._
  import scalaz._, Scalaz._ // Not required, but you'll be glad you did...

  // TODO: Case Classes and Automatic Codec Generation

  // Some grade-A, domain-driven case class action right here.
  case class Person(
                     name: String,
                     age: Int,
                     things: List[String],
                     mother: Option[String]
                   )

  // We bring a `CodecJson[Person]` into scope.
  implicit val personCodec: JsonCodec[Person] = JsonCodec(
    // We need to tell argonaut how to convert between `Person` and `Json`.
    // Argonaut does the hard parts: it just needs us to give it a few hints.
    // We use `casecodec4` because `Person` has four fields.
    argonaut.Argonaut.casecodec4(Person.apply, Person.unapply)(
      "prénom", "âge", "des_choses", "mère"
    ) // Name the properties whatever you'd like.
  )

  // We now have access to
  //
  //     toJson:       Person      => Json
  //     printJson:    Person      => String
  //     prettyJson:   Person      => String
  //     serialize:    Person      => Array[Byte]
  //     deserialize:  Array[Byte] => Result[Person]
  //     parseJson:    String      => Result[Person]
  //     fromJson:     Json        => Result[Person]
  //
  // which satisfy the following contract:
  //
  //     fromJson(toJson(t))        == Result.safely(t)
  //     parseJson(printJson(t))    == Result.safely(t)
  //     parseJson(prettyJson(t))   == Result.safely(t)
  //     deserialize(serialize(t))  == Result.safely(t)
  //
  //     fromJson(json) map toJson flatMap fromJson == fromJson(json)
  //     parseJson(string) map printJson flatMap parseJson == parseJson(string)
  //     parseJson(string) map prettyJson flatMap parseJson == parseJson(string)
  //     deserialize(bytes) map serialize flatMap deserialize == deserialize(bytes)
  //
  // The contract ensure that our implementation is reasonable and coherent.

  // TODO: Basic Usage

  // Create a `Person` as you normally would.
  val tim = Person("Tim Drake", 19, List("Bo"), Some("Janet Drake"))
  assert(
    prettyJson(tim) ==
      """{
        |  "prénom" : "Tim Drake",
        |  "âge" : 19,
        |  "des_choses" : [
        |    "Bo"
        |  ],
        |  "mère" : "Janet Drake"
        |}""".stripMargin
  )

  assert(
    // Our `Person` survives `Json`-ification.
    fromJson[Person](toJson(tim)).contains(tim)
  )
  assert(
    // Our `Person` survives Stringification.
    parseJson[Person](printJson(tim)).contains(tim)
  )
  assert(
    // Our `Person` survives pretty stringification.
    parseJson[Person](prettyJson(tim)).contains(tim)
  )
  assert(
    // Our `Person` survives serialization/deserialization.
    deserialize[Person](serialize(tim)).contains(tim)
  )

  // It's easy to create `Json` values in native Scala (though
  // you often won't need to do any `Json` manipulation like this).
  val batman: Json = Json.obj(
    "prénom" -> "Batman",
    "âge" -> 38,
    "des_choses" -> Json.arr("Batarang", "Batmobile")
    // Notice the "mère" property is absent, which
    // is perfectly fine, since it's an `Option[_]`.
  )

  assert(
    // `Json` can be converted into `Person`.
    fromJson[Person](batman).contains(
      Person("Batman", 38, List("Batarang", "Batmobile"), None)
    )
  )
  assert(
    // `Json` survives `Person`-ification.
    fromJson[Person](batman)
      .map(toJson[Person])
      .flatMap(fromJson[Person])
      == fromJson[Person](batman)
  )

  // What kind of JSON library would be complete without parsing JSON strings?
  val bruce: String =
    """{
      |  "prénom" : "Bruce Wayne",
      |  "âge" : 38,
      |  "des_choses" : [
      |    "Money",
      |    "Alfred"
      |  ],
      |  "mère" : "Martha Wayne"
      |}""".stripMargin

  assert(
    // A JSON string can be parsed into a `Person`.
    parseJson[Person](bruce).contains(
      Person("Bruce Wayne", 38, List("Money", "Alfred"), Some("Martha Wayne"))
    )
  )
  assert({
    // A JSON string can be parsed into a native `Json` value.
    parseJson[Json](bruce).contains(Json.obj(
      "prénom" -> "Bruce Wayne",
      "âge" -> 38,
      "des_choses" -> Json.arr("Money", "Alfred"),
      "mère" -> "Martha Wayne"
    ))
  })
  assert(
    // The `serialize` and `deserialize` methods for `String` still work.
    deserialize[String](serialize(bruce)).contains(bruce)
  )
  assert(
    // Our JSON string survives a round trip.
    parseJson[Person](bruce)
      .map(printJson[Person])
      .flatMap(parseJson[Person])
      == parseJson[Person](bruce)
  )

  // TODO: Validation Guarantees

  // Parsing a `Json` value or a JSON string can fail for a few of reasons:
  // - the data could have the wrong shape, or
  // - the data could have the right shape, but some values could be null.
  // In either of those cases, `serialization` returns `None`, giving users
  // confidence that the returned data is guaranteed to be coherent.

  val agelessPerson: Json = Json.obj(
    "prénom" -> "Ageless",
    "des_choses" -> Json.emptyArr
  )
  assert(
    // "âge" is absent, so parsing should fail gracefully.
    fromJson[Person](agelessPerson).isEmpty
  )
  val nullAgePerson: Json = Json.obj(
    "prénom" -> "Null-age",
    "âge" -> Json.nul,
    "des_choses" -> Json.emptyArr
  )
  assert(
    // "âge" is null, so parsing should fail gracefully.
    fromJson[Person](nullAgePerson).isEmpty
  )

  // Data shape requirements are rather strict:
  // - absent or null properties must be `Optional` in your case class,
  // - absent or null JSON arrays are not read as empty lists, and
  // - absent or null strings are not read as empty strings.
  // Make sure your case class reflects your expectations about your data.
  val noThingsPerson: String =
  """{
    |  "prénom" : "No-things",
    |  "âge" : 5
    |}""".stripMargin
  assert(
    // "des_choses" is missing, so fail.
    parseJson[Person](noThingsPerson).isEmpty
  )
  val nullThingsPerson: String =
    """{
      |  "prénom" : "No-things",
      |  "âge" : 5,
      |  "des_choses" : null
      |}""".stripMargin
  assert(
    // "des_choses" is null, but not an `Option` in our case class, so fail.
    parseJson[Person](nullThingsPerson).isEmpty
  )

  // How does the library (argonaut, really)
  // handle JSON objects with extra fields?
  val augmentedPerson: String =
  """{
    |  "prénom" : "Augie",
    |  "âge" : 18,
    |  "des_choses" : [],
    |  "mains" : 4
    |}""".stripMargin // Augie is an Emacs user, obv.
  assert(
    // Extra fields in JSON strings are silently ignored. (Quack, quack!)
    parseJson[Person](augmentedPerson).contains(
      Person("Augie", 18, List(), None)
    )
  )

  // TODO: Algebraic Data Types

  // How about algebraic sum types?
  sealed trait ADTSum
  case class SBool(getBool: Boolean) extends ADTSum
  case class SInt(getInt: Int) extends ADTSum

  // AFAIK, argonaut does not support automatic codec generation
  // for the above idiom, but we can manually write converters.

  // One direction is always easy.
  def adtSumToJson: ADTSum => Json = {
    case SBool(p) => Json.obj("bool" -> p)
    case SInt(n) => Json.obj("int" -> n)
  }

  // One direction is always hard.
  def adtSumFromJson: Json => Option[ADTSum] =
    json => json.assoc.flatMap { assoc =>
      (assoc.get("bool"), assoc.get("int")) match {
        case (Some(p), None) => p.bool map SBool
        case (None, Some(n)) => n.long flatMap { long =>
          scala.util.Try(long.toInt).toOption } map SInt
        case _ => None
      }
    }

  // Use your converters as inputs to `JsonSerializer.apply`.
  // I usually prefer to inline to converters so as not to have them
  // in global scope (they are redundant with `fromJson` and `toJson`).
  implicit val jsonCodecADTSum: JsonCodec[ADTSum] =
    JsonCodec[ADTSum](adtSumToJson, adtSumFromJson)

  // And everything works as expected.
  assert(
    printJson[ADTSum](SBool(false)) == """{"bool":false}"""
  )
  assert(
    printJson[ADTSum](SInt(12)) == """{"int":12}"""
  )
  assert(
    parseJson[ADTSum]("""{ "bool" : true }""").get == SBool(true)
  )
  assert(
    parseJson[ADTSum]("""{ "int": -2}""").get == SInt(-2)
  )
  assert(
    deserialize[ADTSum](serialize(SInt(51))).contains(SInt(51))
  )

  // TODO: Nested Case Classes

  // Go ahead, make a nested case class.
  case class Key(get: Int)
  case class Value(get: String)
  case class Pair(key: Key, value: Value)

  // The invocation is a bit more complicated.
  // Use a CodecJson rather than a JsonCodec here.
  // The reason is complicated, and the design is questionable,
  // but it'll get better soon ^_^;
  implicit val pairCodec: JsonCodec[Pair] = JsonCodec({
    // Argonaut needs codecs for `Key` and `Value` if we
    // expect it to generate a codec for `Pair`, so we ask
    // it to generate those prerequisite codecs first.
    implicit def keyCodec =
      argonaut.Argonaut.casecodec1(Key.apply, Key.unapply)("get")
    implicit def valueCodec =
      argonaut.Argonaut.casecodec1(Value.apply, Value.unapply)("get")
    argonaut.Argonaut.casecodec2(Pair.apply, Pair.unapply)("key", "value")
  })

  val pair = Pair(Key(5), Value("foo"))

  assert(
    // And it works like a charm, albeit there's the boilerplate.
    printJson(pair) == """{"key":{"get":5},"value":{"get":"foo"}}"""
  )

  // We need to go deeper.
  case class ClassyMap(pairs: List[Pair])

  // Build on top of what we've already built.
  implicit val classyMapCodec: JsonCodec[ClassyMap] = JsonCodec(
    // Argonaut already knows how to encode/decode
    // `Pair`s, so no need to provide evidence here.
    argonaut.Argonaut.casecodec1(ClassyMap.apply, ClassyMap.unapply)("pairs")
  )

  val map = ClassyMap(List(pair))

  // And everything works.
  assert(
    deserialize[ClassyMap](serialize(map)).contains(map)
  )
  assert(
    prettyJson(map) ==
      """{
        |  "pairs" : [
        |    {
        |      "key" : {
        |        "get" : 5
        |      },
        |      "value" : {
        |        "get" : "foo"
        |      }
        |    }
        |  ]
        |}""".stripMargin
  )

  // TODO: Multiple `JsonSerializer[T]`s in Scope

  // We did not need to make our `ClassyMapSerializer` implicit. Any of the
  // objects we made implicitly-available above can alternatively have been
  // created without the `implicit` keyword, at the cost of having to call them
  // directly (e.g. `ClassyMapSerializer.toPrettyJsonString(map)` vs.
  // `toPrettyJsonString(map)`: think of the `implicit` keyword as an import
  // statement, sort-of). You would leave out the `implicit` keyword if you'd
  // like to have access to two or more alternate ways to serialize the same
  // class coexisting in scope.
  val terseClassyMapSerializer: JsonCodec[ClassyMap] =
    JsonCodec[ClassyMap](
      // Provide a `ClassyMap => Json`.
      to = (classyMap: ClassyMap) => Json.array(
        classyMap.pairs.map(pair => Json.obj(
          "k" -> pair.key.get,
          "v" -> pair.value.get
        ))
      ),
      // Provide a `Json => Result[ClassyMap]`.
      from = (json: Json) => for {
        jsons <- json.array
        assocs <- jsons.map(_.assoc).sequence // This is why we like scalaz.
        pairs <- assocs.map(obj => for {
          kJson <- obj.get("k")
          kNum <- kJson.number
          k <- scala.util.Try(kNum.toIntExact).toOption.flatMap(Option.apply)
          vJson <- obj.get("v")
          v <- vJson.string
        } yield Pair(Key(k), Value(v))).sequence
      } yield ClassyMap(pairs)
    )

  val mapBytes = serialize(map)
  val mapBytesTerse = terseClassyMapSerializer.serialize(map)
  assert(
    // In UTF-8, `mapBytesTerse` is 19 bytes, and `mapBytes` is 51.
    mapBytesTerse.length < mapBytes.length
  )

  // Multiple serializers coexisting in scope.
  assert(
    // Native scala representation:
    map.toString ==
      """ClassyMap(List(Pair(Key(5),Value(foo))))"""
  )
  assert(
    // Verbose JSON serializer:
    printJson(map) ==
      """{"pairs":[{"key":{"get":5},"value":{"get":"foo"}}]}"""
  )
  assert(
    // Terse JSON serializer:
    terseClassyMapSerializer.printJson(map) ==
      """[{"k":5,"v":"foo"}]"""
  )

  // TODO: Case Classes with Type Parameters

  // Let's make a case class with a type parameter.
  case class Wrapper[T](runWrapper: T)

  // We declare an `implicit def` (rather than an `implicit
  // object`) because we need to parametrize by the type `T`.
  implicit def wrapperSerializer[T](
                                     implicit codecT: JsonCodec[T]
                                   ): JsonCodec[Wrapper[T]] = JsonCodec(
    // The `casecodecN` family of functions each have
    // N+1 type parameters and three argument groups.
    //
    //     // `A` is a type we assume we can encode/decode to JSON.
    //     // `X` is the type we'd like to be able to encode/decode.
    //     casecodec1[A, X](
    //       con: A => X,          // A constructor for `X`.
    //       ext: X => Option[A]   // An extractor for `X`.
    //     )(
    //       prop: String          // Name we'd like for our JSON property.
    //     )(
    //       implicit
    //       encA: EncodeJson[A],  // A type enforcing half of our precondition.
    //       decA: DecodeJson[A]   // The other half of our precondition.
    //     ): CodecJson[X]         // Extends `EncodeJson[X]`, `DecodeJson[X]`.
    //
    // The return type of `casecodec1` allows us to encode/decode values of `X`.
    // That the last argument group is implicit means that scalac will attempt
    // to resolve `encA` and `decA` at compile time; they will resolve if there
    // is exactly one value of type `EncodeJson[A]` (res. `DecodeJson[A]`) in
    // implicit scope, producing a compiler error if there are none or more than
    // one. We can at any time override the implicits by explicitly providing
    // `encA` and `decA` (as we do below).

    argonaut.Argonaut.casecodec1[T, Wrapper[T]](
      Wrapper.apply[T], Wrapper.unapply[T]
    )("runWrapper")
  )

  // We had to read a lot, but we only ended up needing to writing three lines
  // of code, so that's pretty cool. Now we can serialize and deserialize any
  // `Wrapper[T]` so long as `T` is serializable/deserializable.
  assert(
    deserialize[Wrapper[Person]](serialize(Wrapper(Person("", 0, Nil, None))))
      .get.runWrapper == Person("", 0, Nil, None)
  )
  assert(
    prettyJson(Wrapper(tim)) ==
      """{
        |  "runWrapper" : {
        |    "prénom" : "Tim Drake",
        |    "âge" : 19,
        |    "des_choses" : [
        |      "Bo"
        |    ],
        |    "mère" : "Janet Drake"
        |  }
        |}""".stripMargin
  )
  assert(
    prettyJson(Wrapper(map)) ==
      """{
        |  "runWrapper" : {
        |    "pairs" : [
        |      {
        |        "key" : {
        |          "get" : 5
        |        },
        |        "value" : {
        |          "get" : "foo"
        |        }
        |      }
        |    ]
        |  }
        |}""".stripMargin
  )

  // For future reference, `casecodec2` has signature
  //
  //     casecodec2[A1, A2, X](
  //       con: (A1, A2) => X,
  //       ext: X => Option[(A1, A2)]
  //     )(
  //       prop1: String,
  //       prop2: String
  //     )(
  //       implicit
  //       encA1: EncodeJson[A1],
  //       decA1: DecodeJson[A1],
  //       encA2: EncodeJson[A2],
  //       decA2: DecodeJson[A2]
  //     ): CodecJson[X]
  //
  // and `casecodec3` has signature ...
  //
  // Argonaut provides up to `casecodec22`, after
  // which you have to start writing them yourself.
  // But don't worry, I believe in you.

  println(deserialize[String](Array[Byte](10, 32, 32, 40, 32, 32, 83, 99, 97,
    108, 97, 32, 32, 41, 10, 10, 32, 32, 110, 111, 119, 32, 32, 121, 111, 117,
    39, 114, 101, 10, 32, 32, 32, 32, 112, 108, 97, 121, 105, 110, 103, 10, 32,
    32, 119, 105, 116, 104, 32, 32, 112, 111, 119, 101, 114, 46, 10)).get)
}
