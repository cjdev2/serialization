#!/bin/sh
exec scala "$0" "$@"
!#

def generateJsonCodecTupleN(n: Int): String = {

  val types: String = (1 to n).map(i => s"T$i").mkString(", ")

  val typesWithConstraints: String =
    (1 to n).map(i => s"T$i: JsonCodec").mkString(", ")

  val toJsonLines: String = (1 to n).map {
    i => s"""    "_$i" -> json.toJson[T$i](t._$i)"""
  }.mkString(",\n")

  val fromJsonLines: String = (1 to n).map {
    i => s"""    _$i <- (j ~> "_$i").flatMap(json.fromJson[T$i])"""
  }.mkString("\n")

  val components: String = (1 to n).map(i => s"_$i").mkString(", ")

  s"""/**
     |  * Implements [[JsonCodec]] for `Tuple$n` whenever the component types
     |  * have instances of [[JsonCodec]] in implicit scope.
     |  */
     |implicit def jsonCodecTuple$n[$typesWithConstraints]:
     |JsonCodec[($types)] = new JsonCodec[($types)] {
     |  import JsonImplicits._
     |  def toJson(t: ($types)): Json = Json.obj(
     |$toJsonLines
     |  )
     |  def fromJson(j: Json): Option[($types)] = for {
     |$fromJsonLines
     |  } yield ($components)
     |}
     |""".stripMargin
}

(2 to 7).foreach { i => println(generateJsonCodecTupleN(i)) }
