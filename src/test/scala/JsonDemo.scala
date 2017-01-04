object JsonDemo extends App {

  import com.cj.serialization._
  import com.cj.serialization.json._

  import argonaut.{Argonaut, Json}

  // some grade-A, domain-driven case class action right here
  case class Person(
                     name: String,
                     age: Int,
                     things: List[String],
                     mother: Option[String]
                   )

  // we bring a `Person` serializer into scope
  implicit object PersonSerializer extends JsonSerializerFromCodec[Person](
    // we need to tell argonaut how to convert between `Person` and `Json`
    // argonaut does the hard parts, it just needs us to give it a few hints
    Argonaut.casecodec4
      (Person.apply, Person.unapply)
      ("prénom", "âge", "des_choses", "mère") // name the fields whatever
    // we use `casecodec4` because `Person` has four fields
  )

  // we now have access to
  //
  //     toJson:             Person      => Json
  //     toJsonString:       Person      => String
  //     toPrettyJsonString: Person      => String
  //     serialize:          Person      => Array[Byte]
  //     deserialize:        Array[Byte] => Option[Person]
  //     fromJsonString:     String      => Option[Person]
  //     fromJson:           Json        => Option[Person]
  //
  // which satisfy the following contract
  //
  //     fromJson(toJson(t))                   == Some(t)
  //     fromJsonString(toJsonString(t))       == Some(t)
  //     fromJsonString(toPrettyJsonString(t)) == Some(t)
  //     deserialize(serialize(t))             == Some(t)
  //
  //     fromJson(json).map(toJson).flatMap(fromJson)                           == fromJson(json)
  //     fromJsonString(string).map(toJsonString).flatMap(fromJsonString)       == fromJsonString(string)
  //     fromJsonString(string).map(toPrettyJsonString).flatMap(fromJsonString) == fromJsonString(string)
  //     deserialize(bytes).map(serialize).flatMap(deserialize)                 == deserialize(bytes)
  //
  // the contract ensure that our implementation is reasonable and coherent,
  // e.g. two different pathways for deserializing a given message will never
  // result in different outcomes

  // create a `Person` as you normally would
  val person1: Person = Person(
    "Tim Drake",
    19,
    List("Bo"),
    Some("Janet Drake")
  )

  assert(
    // our `Person` survives `Json`-ification
    fromJson[Person](toJson(person1)).contains(person1)
  )
  assert(
    // our `Person` survives Stringification
    fromJsonString[Person](toJsonString(person1)).contains(person1)
  )
  assert(
    // our `Person` survives pretty stringification
    fromJsonString[Person](toPrettyJsonString(person1)).contains(person1)
  )
  assert(
    // our `Person` survives serialization/deserialization
    deserialize[Person](serialize(person1)).contains(person1)
  )

  // it's easy to create `Json` values in native Scala (though you often won't
  // need to do any `Json` manipulation like this)
  val person2: Json = Json(
    "prénom" -> Json.jString("Batman"),
    "âge" -> Json.jNumber(38),
    "des_choses" -> Json.array(
      Json.jString("Batarang"),
      Json.jString("Batmobile")
    )
    // notice the "mère" property is absent
  )

  assert(
    // `Json` can be converted into `Person`
    fromJson[Person](person2).contains(
      Person("Batman", 38, List("Batarang", "Batmobile"), None)
    )
  )
  assert(
    // `Json` survives `Person`-ification
    fromJson[Person](person2)
      .map(toJson[Person])
      .flatMap(fromJson[Person])
      == fromJson[Person](person2)
  )

  // what kind of JSON library would be complete without parsing JSON strings?
  val person3: String =
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
    // A JSON string can be parsed into a `Person`
    fromJsonString[Person](person3).contains(
      Person("Bruce Wayne", 38, List("Money", "Alfred"), Some("Martha Wayne"))
    )
  )
  assert({
    // A JSON string can be parsed into a native `Json`
    deserialize[Json](serialize(person3)).contains(Json(
      "prénom" -> Json.jString("Bruce Wayne"),
      "âge" -> Json.jNumber(38),
      "des_choses" -> Json.array(
        Json.jString("Money"),
        Json.jString("Alfred")
      ),
      "mère" -> Json.jString("Martha Wayne")
    ))
  })
  assert(
    // of course the `serialize` and `deserialize` methods for `String` still
    // work as we'd expect
    deserialize[String](serialize(person3)).contains(person3)
  )
  assert(
    // our JSON string survives a round trip
    fromJsonString[Person](person3)
      .map(toJsonString[Person])
      .flatMap(fromJsonString[Person])
      == fromJsonString[Person](person3)
  )

  // go ahead, make a nested case class
  case class Key(get: Int)
  case class Value(get: String)
  case class Pair(key: Key, value: Value)

  // the invocation is a bit more complicated
  implicit object PairSerializer extends JsonSerializerFromCodec[Pair]({
    implicit def keyCodec =
      Argonaut.casecodec1(Key.apply, Key.unapply)("get")
    implicit def valueCodec =
      Argonaut.casecodec1(Value.apply, Value.unapply)("get")
    Argonaut.casecodec2(Pair.apply, Pair.unapply)("key", "value")
  })

  val pair: Pair = Pair(Key(5), Value("foo"))

  assert(
    // but it works like a charm
    toJsonString(pair) == """{"key":{"get":5},"value":{"get":"foo"}}"""
  )
}
