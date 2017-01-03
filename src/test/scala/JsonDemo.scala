object JsonDemo extends App {

  import com.cj.serialization._
  import com.cj.serialization.json._

  import argonaut.{Json, Argonaut}

  case class Person(name: String, age: Int, things: List[String])

  // we bring a `Person` serializer into scope
  implicit object PersonSerializer extends JsonSerializerFromCodec[Person](
    // we need to tell argonaut how to convert between `Person` and `Json`
    // argonaut does the hard parts, it just needs us to give it a few hints
    Argonaut.casecodec3(Person.apply, Person.unapply)("prénom", "âge", "choses")
    // we use `casecodec3` because `Person` has three fields
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
  // which satisfy the following contracts
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
  // the laws ensure that our implementation is reasonable and coherent, e.g.
  // two different pathways for deserializing a given message will never result
  // in different outcomes

  val person1: Person = Person("Bam Bam", 2, List("club"))

  // it's easy to create `Json` values
  val person2: Json = Json.obj(
    "prénom" -> Json.jString("Batman"),
    "âge" -> Json.jNumber(38),
    "choses" -> Json.array(
      Json.jString("Batarang"),
      Json.jString("Batmobile")
    )
  )

  // of course we can also parse JSON strings
  val person3: String =
    """{
      |  "prénom" : "Bruce Wayne",
      |  "âge" : 38,
      |  "choses" : [
      |    "Money",
      |    "Alfred"
      |  ]
      |}""".stripMargin

  println("\n# person1")

  println(
    // Native scala case class
    person1
  )
  println(
    // JSON string
    toJsonString(person1)
  )
  println(
    // Pretty-printed JSON string
    toPrettyJsonString(person1)
  )
  println(
    // Contract: fromJsonString(toJsonString(t)) == Some(t)
    fromJsonString[Person](toJsonString(person1))
  )
  println(
    // Contract: fromJsonString(toPrettyJsonString(t)) == Some(t)
    fromJsonString[Person](toPrettyJsonString(person1))
  )
  println(
    // Contract: deserialize(serialize(t)) == Some(t)
    deserialize[Person](serialize(person1))
  )

  println("\n# person2")

  println(
    // Argonaut's `Json` native scala type
    person2
  )
  println(
    // extract `Person` from `Json`
    fromJson[Person](person2)
  )
  println(
    // Contract: fromJson(json).map(toJson).flatMap(fromJson) == fromJson(json)
    fromJson[Person](person2)
      .map(toJson[Person])
      .flatMap(fromJson[Person])
  )

  println("\n# person3")

  println(
    // JSON string
    person3
  )
  println(
    // extract `Person` from JSON string
    fromJsonString[Person](person3)
  )
  println(
    // Contract:
    //  fromJsonString(string).map(toJsonString).flatMap(fromJsonString)
    //  == fromJsonString(string)
    fromJsonString[Person](person3)
      .map(toJsonString[Person])
      .flatMap(fromJsonString[Person])
  )
}
