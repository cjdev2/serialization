import org.scalatest.{FlatSpec, Matchers}

class YamlSpecTest extends FlatSpec with Matchers {

  import com.cj.serialization.yaml.Yaml
  import com.cj.serialization.yaml.YamlImplicits._

  behavior of "Yaml.parse"

  it should "correctly parse sequence of scalars" in {
    // given
    val raw: String =
      """- Mark McGwire
        |- Sammy Sosa
        |- Ken Griffey""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.arr(
      "Mark McGwire",
      "Sammy Sosa",
      "Ken Griffey"
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse mapping scalars to scalars" in {
    // given
    val raw: String =
      """hr:  65    # Home runs
        |avg: 0.278 # Batting average
        |rbi: 147   # Runs Batted In""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.obj(
      "hr" -> 65,
      "avg" -> 0.278,
      "rbi" -> 147
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse mapping scalars to sequences" in {
    // given
    val raw: String =
      """american:
        |  - Boston Red Sox
        |  - Detroit Tigers
        |  - New York Yankees
        |national:
        |  - New York Mets
        |  - Chicago Cubs
        |  - Atlanta Braves
        |""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.obj(
      "american" -> Yaml.arr(
        "Boston Red Sox",
        "Detroit Tigers",
        "New York Yankees"
      ),
      "national" -> Yaml.arr(
        "New York Mets",
        "Chicago Cubs",
        "Atlanta Braves"
      )
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse sequences of mappings" in {
    // given
    val raw: String =
      """-
        |  name: Mark McGwire
        |  hr:   65
        |  avg:  0.278
        |-
        |  name: Sammy Sosa
        |  hr:   63
        |  avg:  0.288
        |""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.arr(
      Yaml.obj(
        "name" -> "Mark McGwire",
        "hr" -> 65,
        "avg" -> 0.278
      ),
      Yaml.obj(
        "name" -> "Sammy Sosa",
        "hr" -> 63,
        "avg" -> 0.288
      )
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse sequence of sequences" in {
    // given
    val raw: String =
      """- [name        , hr, avg  ]
        |- [Mark McGwire, 65, 0.278]
        |- [Sammy Sosa  , 63, 0.288]
        |""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.arr(
      Yaml.arr("name", "hr", "avg"),
      Yaml.arr("Mark McGwire", 65, 0.278),
      Yaml.arr("Sammy Sosa", 63, 0.288)
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse mapping of mappings" in {
    // given
    val raw: String =
      """Mark McGwire: {hr: 65, avg: 0.278}
        |Sammy Sosa: {
        |    hr: 63,
        |    avg: 0.288
        |  }
        |""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.obj(
      "Mark McGwire" -> Yaml.obj("hr" -> 65, "avg" -> 0.278),
      "Sammy Sosa" -> Yaml.obj("hr" -> 63, "avg" -> 0.288)
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse two documents in a stream" in {
    // given
    val raw: String =
      """# Ranking of 1998 home runs
        |---
        |- Mark McGwire
        |- Sammy Sosa
        |- Ken Griffey
        |
        |# Team ranking
        |---
        |- Chicago Cubs
        |- St Louis Cardinals
        |""".stripMargin

    val expected: Option[Yaml] =
      Some(Yaml.stream(Stream(
        Yaml.arr(
          "Mark McGwire",
          "Sammy Sosa",
          "Ken Griffey"
        ),
        Yaml.arr(
          "Chicago Cubs",
          "St Louis Cardinals"
        )
      )))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  ignore should "correctly parse play by play feed from a game" in {
    // TODO: SnakeYaml, unfortunately, interprets timestrings as seconds after midnight
    // given
    val raw: String =
      """---
        |time: 20:03:20
        |player: Sammy Sosa
        |action: strike (miss)
        |...
        |---
        |time: 20:03:47
        |player: Sammy Sosa
        |action: grand slam
        |...
        |""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.stream(Stream(
      Yaml.obj(
        "time" -> "20:03:20",
        "player" -> "Sammy Sosa",
        "action" -> "strike (miss)"
      ),
      Yaml.obj(
        "time" -> "20:03:47",
        "player" -> "Sammy Sosa",
        "action" -> "grand slam"
      )
    )))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse Single Document with Two Comments" in {
    // given
    val raw: String =
      """---
        |hr: # 1998 hr ranking
        |  - Mark McGwire
        |  - Sammy Sosa
        |rbi:
        |  # 1998 rbi ranking
        |  - Sammy Sosa
        |  - Ken Griffey
        |""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.obj(
      "hr" -> Yaml.arr("Mark McGwire", "Sammy Sosa"),
      "rbi" -> Yaml.arr("Sammy Sosa", "Ken Griffey")
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse Node for “Sammy Sosa” appears twice in this document" in {
    // given
    val raw: String =
      """---
        |hr:
        |  - Mark McGwire
        |  # Following node labeled SS
        |  - &SS Sammy Sosa
        |rbi:
        |  - *SS # Subsequent occurrence
        |  - Ken Griffey
        |""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.obj(
      "hr" -> Yaml.arr("Mark McGwire", "Sammy Sosa"),
      "rbi" -> Yaml.arr("Sammy Sosa", "Ken Griffey")
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  ignore should "correctly parse Mapping between Sequences" in {
    // TODO: SnakeYaml, unfortunately, interprets datestrings as java datetimes
    // given
    val raw: String =
      """? - Detroit Tigers
        |  - Chicago Cubs
        |:
        |  - 2001-07-23
        |
        |? [ New York Yankees,
        |    Atlanta Braves ]
        |: [ 2001-07-02, 2001-08-12,
        |    2001-08-14 ]
        |""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.assoc(Map(
      Yaml.arr("Detroit Tigers", "Chicago Cubs") ->
        Yaml.arr("2001-07-23"),
      Yaml.arr("New York Yankees", "Atlanta Braves") ->
        Yaml.arr("2001-07-02", "2001-08-12", "2001-08-14")
    )))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse Compact Nested Mapping" in {
    // given
    val raw: String =
      """---
        |# Products purchased
        |- item    : Super Hoop
        |  quantity: 1
        |- item    : Basketball
        |  quantity: 4
        |- item    : Big Shoes
        |  quantity: 1
        |""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.arr(
      Yaml.obj(
        "item" -> "Super Hoop",
        "quantity" -> 1
      ),
      Yaml.obj(
        "item" -> "Basketball",
        "quantity" -> 4
      ),
      Yaml.obj(
        "item" -> "Big Shoes",
        "quantity" -> 1
      )
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse In literals, newlines are preserved" in {
    // given
    val raw: String =
      """# ASCII Art
        |--- |
        |  \//||\/||
        |  // ||  ||__
        |""".stripMargin

    val expected: Option[Yaml] =
      Some(Yaml.string("\\//||\\/||\n// ||  ||__\n"))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse In the folded scalars, newlines become spaces" in {
    // given
    val raw: String =
      """--- >
        |  Mark McGwire's
        |  year was crippled
        |  by a knee injury.
        |""".stripMargin

    val expected: Option[Yaml] =
      Some(Yaml.string("Mark McGwire's year was crippled by a knee injury.\n"))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse Folded newlines are preserved for \"more indented\" and blank lines" in {
    // given
    val raw: String =
      """>
        | Sammy Sosa completed another
        | fine season with great stats.
        |
        |   63 Home Runs
        |   0.288 Batting Average
        |
        | What a year!
        |""".stripMargin

    val expected: Option[Yaml] = Some(
      Yaml.string("Sammy Sosa completed another fine season with great stats.\n\n  63 Home Runs\n  0.288 Batting Average\n\nWhat a year!\n"
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse Indentation determines scope" in {
    // given
    val raw: String =
      """name: Mark McGwire
        |accomplishment: >
        |  Mark set a major league
        |  home run record in 1998.
        |stats: |
        |  65 Home Runs
        |  0.278 Batting Average
        |""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.obj(
      "name" -> "Mark McGwire",
      "accomplishment" -> "Mark set a major league home run record in 1998.\n",
      "stats" -> "65 Home Runs\n0.278 Batting Average\n"
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse Quoted Scalars" in {
    // given
    val raw: String =
      """unicode: "Sosa did fine.\u263A"
        |control: "\b1998\t1999\t2000\n"
        |hex esc: "\x0d\x0a is \r\n"
        |
        |single: '"Howdy!" he cried.'
        |quoted: ' # Not a ''comment''.'
        |tie-fighter: '|\-*-/|'
        |""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.obj(
      "unicode" -> "Sosa did fine.\u263A",
      "control" -> "\b1998\t1999\t2000\n",
      "hex esc" -> "\r\n is \r\n",
      "single" -> "\"Howdy!\" he cried.",
      "quoted" -> " # Not a 'comment'.",
      "tie-fighter" -> "|\\-*-/|"
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse Multi-line Flow Scalars" in {
    // given
    val raw: String =
      """plain:
        |  This unquoted scalar
        |  spans many lines.
        |
        |quoted: "So does this
        |  quoted scalar.\n"
        |""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.obj(
      "plain" -> "This unquoted scalar spans many lines.",
      "quoted" -> "So does this quoted scalar.\n"
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  it should "correctly parse Yaml maps with complex keys" in {

    // given
    val raw: String = """{? ["yo", "dawg"]: "i heard you like yaml"}"""

    val expected: Option[Yaml] = Some(Yaml.assoc(Map(
      (Yaml.arr("yo", "dawg"), Yaml.string("i heard you like yaml"))
    )))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  behavior of "Yaml.print"

  it should "correctly print simple yaml sequences" in {
    // given
    val yaml: Yaml = Yaml.arr(
      "Mark McGwire",
      "Sammy Sosa",
      "Ken Griffey"
    )

    val expected: String = """["Mark McGwire", "Sammy Sosa", "Ken Griffey"]"""

    // when
    val actual: String = yaml.print

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  it should "correctly print simple yaml mappings" in {
    // given
    val yaml: Yaml = Yaml.obj(
      "hr" -> 65,
      "avg" -> 0.278,
      "rbi" -> 147
    )

    val expected: String = """{"hr": 65, "avg": 0.278, "rbi": 147}"""

    // when
    val actual: String = yaml.print

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  it should "correctly print simple yaml streams" in {
    // given
    val yaml: Yaml = Yaml.stream(Stream(
      Yaml.obj(
        "time" -> "20:03:20",
        "player" -> "Sammy Sosa",
        "action" -> "strike (miss)"
      ),
      Yaml.obj(
        "time" -> "20:03:47",
        "player" -> "Sammy Sosa",
        "action" -> "grand slam"
      )
    ))

    val expected: String =
      """{"time": "20:03:20", "player": "Sammy Sosa", "action": "strike (miss)"}
        |...
        |{"time": "20:03:47", "player": "Sammy Sosa", "action": "grand slam"}
        |...
        |""".stripMargin

    // when
    val actual: String = yaml.print

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  it should "correctly print yaml maps with non-string keys" in {
    // given
    val yaml: Yaml =
      Yaml.assoc(Map(Yaml.arr("foo", "bar") -> Yaml.string("baz")))

    val expected: String =
      """{? ["foo", "bar"]: "baz"}"""

    // when
    val actual: String = yaml.print

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  behavior of "Yaml.pretty"

  it should "correctly print simple yaml sequences" in {
    // given
    val yaml: Yaml = Yaml.arr(
      "Mark McGwire",
      "Sammy Sosa",
      "Ken Griffey"
    )

    val expected: String =
      """- Mark McGwire
        |- Sammy Sosa
        |- Ken Griffey
        |""".stripMargin

    // when
    val actual: String = yaml.pretty

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  it should "correctly print simple yaml mappings" in {
    // given
    val yaml: Yaml = Yaml.obj(
      "hr" -> 65,
      "avg" -> 0.278,
      "rbi" -> 147
    )

    val expected: String =
      """hr: 65
        |avg: 0.278
        |rbi: 147
        |""".stripMargin

    // when
    val actual: String = yaml.pretty

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  it should "correctly print mapping scalars to sequences" in {
    // given
    val expected: String =
      """american:
        |  - Boston Red Sox
        |  - Detroit Tigers
        |  - New York Yankees
        |national:
        |  - New York Mets
        |  - Chicago Cubs
        |  - Atlanta Braves
        |""".stripMargin

    val yaml: Yaml = Yaml.obj(
      "american" -> Yaml.arr("Boston Red Sox", "Detroit Tigers", "New York Yankees"),
      "national" -> Yaml.arr("New York Mets", "Chicago Cubs", "Atlanta Braves")
    )

    // when
    val actual: String = yaml.pretty

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  it should "correctly print sequences of mappings" in {
    // given
    val expected: String =
      """-
        |  name: Mark McGwire
        |  hr: 65
        |  avg: 0.278
        |-
        |  name: Sammy Sosa
        |  hr: 63
        |  avg: 0.288
        |""".stripMargin

    val yaml: Yaml = Yaml.arr(
      Yaml.obj(
        "name" -> "Mark McGwire",
        "hr" -> 65,
        "avg" -> 0.278
      ),
      Yaml.obj(
        "name" -> "Sammy Sosa",
        "hr" -> 63,
        "avg" -> 0.288
      )
    )

    // when
    val actual: String = yaml.pretty

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  it should "correctly print simple yaml streams" in {
    // given
    val yaml: Yaml = Yaml.stream(Stream(
      Yaml.obj(
        "time" -> "20:03:20",
        "player" -> "Sammy Sosa",
        "action" -> "strike (miss)"
      ),
      Yaml.obj(
        "time" -> "20:03:47",
        "player" -> "Sammy Sosa",
        "action" -> "grand slam"
      )
    ))

    val expected: String =
      """time: 20:03:20
        |player: Sammy Sosa
        |action: strike (miss)
        |...
        |time: 20:03:47
        |player: Sammy Sosa
        |action: grand slam
        |...
        |""".stripMargin

    // when
    val actual: String = yaml.pretty

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  it should "correctly print yaml maps with non-string keys" in {
    // given
    val yaml: Yaml = Yaml.assoc(Map(
      Yaml.arr("foo", "bar") -> Yaml.string("baz")
    ))

    val expected: String =
      """?
        |  - foo
        |  - bar
        |: baz
        |""".stripMargin

    // when
    val actual: String = yaml.pretty

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  behavior of "Yaml.printJson"

  it should "correctly print simple yaml sequences" in {
    // given
    val yaml: Yaml = Yaml.arr(
      "Mark McGwire",
      "Sammy Sosa",
      "Ken Griffey"
    )

    val expected: Option[String] =
      Some("""["Mark McGwire", "Sammy Sosa", "Ken Griffey"]""")

    // when
    val actual: Option[String] = yaml.printJson

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  it should "correctly print simple yaml mappings" in {
    // given
    val yaml: Yaml = Yaml.obj(
      "hr" -> 65,
      "avg" -> 0.278,
      "rbi" -> 147
    )

    val expected: Option[String] =
      Some("""{"hr": 65, "avg": 0.278, "rbi": 147}""")

    // when
    val actual: Option[String] = yaml.printJson

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  it should "correctly print mapping scalars to sequences" in {
    // given
    val expected: Option[String] =
      Some("""{"american": ["Boston Red Sox", "Detroit Tigers", "New York Yankees"], "national": ["New York Mets", "Chicago Cubs", "Atlanta Braves"]}""")

    val yaml: Yaml = Yaml.obj(
      "american" -> Yaml.arr("Boston Red Sox", "Detroit Tigers", "New York Yankees"),
      "national" -> Yaml.arr("New York Mets", "Chicago Cubs", "Atlanta Braves")
    )

    // when
    val actual: Option[String] = yaml.printJson

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  it should "correctly print sequences of mappings" in {
    // given
    val expected: Option[String] =
      Some("""[{"name": "Mark McGwire", "hr": 65, "avg": 0.278}, {"name": "Sammy Sosa", "hr": 63, "avg": 0.288}]""")

    val yaml: Yaml = Yaml.arr(
      Yaml.obj(
        "name" -> "Mark McGwire",
        "hr" -> 65,
        "avg" -> 0.278
      ),
      Yaml.obj(
        "name" -> "Sammy Sosa",
        "hr" -> 63,
        "avg" -> 0.288
      )
    )

    // when
    val actual: Option[String] = yaml.printJson

    // then
    println(s"Expected\n--------\n$expected\n")
    println(s"Actual\n------\n$actual\n")
    actual should be(expected)
  }

  it should "return Failure for simple yaml streams" in {
    // given
    val yaml: Yaml = Yaml.stream(Stream(
      Yaml.obj(
        "time" -> "20:03:20",
        "player" -> "Sammy Sosa",
        "action" -> "strike (miss)"
      ),
      Yaml.obj(
        "time" -> "20:03:47",
        "player" -> "Sammy Sosa",
        "action" -> "grand slam"
      )
    ))

    // when
    val result: Option[String] = yaml.printJson

    // then
    result.isEmpty should be(true)
  }

  it should "return Failure for yaml maps with non-string keys" in {
    // given
    val yaml: Yaml = Yaml.assoc(Map(
      Yaml.arr("foo", "bar") -> Yaml.string("baz")
    ))

    // when
    val result: Option[String] = yaml.printJson

    // then
    result.isEmpty should be(true)
  }
}
