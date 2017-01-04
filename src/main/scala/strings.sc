import com.cj.serialization._

val stringWithCharacter: String =
  """{
    |  "name" : "Daniel",
    |  "favorite_game" : "¡Pokémon Snap!"
    |}
  """.stripMargin


val bytes: Array[Byte] = stringWithCharacter.getBytes("UTF-8")

val unbytes: String = new String(bytes, "UTF-8")

unbytes == stringWithCharacter

val myInt: Option[Int] = scala.util.Try(5).toOption

