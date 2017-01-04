object JsonDemo extends App {

  import com.cj.serialization._
  import com.cj.serialization.json._

  import argonaut.{Argonaut, Json}

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
      ("name", "age", "things", "mother")
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
  //     fromJson(t).map(toJson).flatMap(fromJson)                         == fromJson(t)
  //     fromJsonString(t).map(toJsonString).flatMap(fromJsonString)       == fromJsonString(t)
  //     fromJsonString(t).map(toPrettyJsonString).flatMap(fromJsonString) == fromJsonString(t)
  //     deserialize(t).map(serialize).flatMap(deserialize)                == deserialize(t)
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
  val person2: Json = Json.obj(
    "name" -> Json.jString("Batman"),
    "age" -> Json.jNumber(38),
    "things" -> Json.array(
      Json.jString("Batarang"),
      Json.jString("Batmobile")
    )
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
      |  "name" : "Bruce Wayne",
      |  "age" : 38,
      |  "things" : [
      |    "Money",
      |    "Alfred"
      |  ],
      |  "mother" : "Martha Wayne"
      |}""".stripMargin

  assert(
    // A JSON string can be parsed into a `Person`
    fromJsonString[Person](person3).contains(
      Person("Bruce Wayne", 38, List("Money", "Alfred"), Some("Martha Wayne"))
    )
  )

  assert({
    // A JSON string can be parsed into a native `Json`
    deserialize[Json](serialize(person3)).contains(Json.obj(
      "name" -> Json.jString("Bruce Wayne"),
      "age" -> Json.jNumber(38),
      "things" -> Json.array(
        Json.jString("Money"),
        Json.jString("Alfred")
      ),
      "mother" -> Json.jString("Martha Wayne")
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
}
