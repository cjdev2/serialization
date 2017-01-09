object JsonDemoMinimal extends App {

  // It's come to my attention that some don't like polluting their global scope
  // with all kinds of implicits. More to the point, if you create an implicit
  // `JsonSerializer` for `Person` and another implicit `Serializer[Person]`
  // (say for Avro), then `serialize` becomes undecidable (a compile-time error
  // at least, but a frustrating one if you don't know what's going on behind
  // the scenes.
  //
  // If you are running into the above problem, one solution is to avoid using
  // underscore, "_", in your import statements and to declare your serializers
  // explicit instead of implicit. This short demo illustrates those practices.

  import com.cj.serialization.json.JsonSerializerFromCodec

  case class Person(
                     name: String,
                     age: Int,
                     things: List[String],
                     mother: Option[String]
                   )

  object PersonS extends JsonSerializerFromCodec[Person](
    argonaut.Argonaut.casecodec4(Person.apply, Person.unapply)(
      "name", "age", "things", "mother"
    )
  )

  val tim = Person("Tim Drake", 19, List("Bo"), Some("Janet Drake"))
  assert(
    PersonS.toPrettyJsonString(tim) ==
      """{
        |  "name" : "Tim Drake",
        |  "age" : 19,
        |  "things" : [
        |    "Bo"
        |  ],
        |  "mother" : "Janet Drake"
        |}""".stripMargin
  )

  val batmanString =
    """{"name":"Batman","age":38,"things":["Batarang","Batmobile"]}"""
  assert(
    PersonS.fromJsonString(batmanString).contains(
      Person("Batman", 38, List("Batarang", "Batmobile"), None)
    )
  )
}
