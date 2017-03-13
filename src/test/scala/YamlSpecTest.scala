import org.scalatest.{FlatSpec, Matchers}

class YamlSpecTest extends FlatSpec with Matchers {

  import com.cj.serialization.yaml.Yaml

  behavior of "Yaml.parse"

  it should "correctly parse sequence of scalars" in {
    // given
    val raw: String =
      """- Mark McGwire
        |- Sammy Sosa
        |- Ken Griffey""".stripMargin

    val expected: Option[Yaml] = Some(Yaml.seq(List(
      Yaml.string("Mark McGwire"),
      Yaml.string("Sammy Sosa"),
      Yaml.string("Ken Griffey")
    )))

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

    val expected: Option[Yaml] = Some(Yaml.map(Map(
      Yaml.string("hr") -> Yaml.int(65),
      Yaml.string("avg") -> Yaml.float(0.278),
      Yaml.string("rbi") -> Yaml.int(147)
    )))

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

    val expected: Option[Yaml] = Some(Yaml.map(Map(
      Yaml.string("american") -> Yaml.seq(List(
        Yaml.string("Boston Red Sox"),
        Yaml.string("Detroit Tigers"),
        Yaml.string("New York Yankees")
      )),
      Yaml.string("national") -> Yaml.seq(List(
        Yaml.string("New York Mets"),
        Yaml.string("Chicago Cubs"),
        Yaml.string("Atlanta Braves")
      ))
    )))

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

    val expected: Option[Yaml] = Some(Yaml.seq(List(
      Yaml.map(Map(
        Yaml.string("name") -> Yaml.string("Mark McGwire"),
        Yaml.string("hr") -> Yaml.int(65),
        Yaml.string("avg") -> Yaml.float(0.278)
      )),
      Yaml.map(Map(
        Yaml.string("name") -> Yaml.string("Sammy Sosa"),
        Yaml.string("hr") -> Yaml.int(63),
        Yaml.string("avg") -> Yaml.float(0.288)
      ))
    )))

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

    val expected: Option[Yaml] = Some(Yaml.seq(List(
      Yaml.seq(List(
        Yaml.string("name"),
        Yaml.string("hr"),
        Yaml.string("avg")
      )),
      Yaml.seq(List(
        Yaml.string("Mark McGwire"),
        Yaml.int(65),
        Yaml.float(0.278)
      )),
      Yaml.seq(List(
        Yaml.string("Sammy Sosa"),
        Yaml.int(63),
        Yaml.float(0.288)
      ))
    )))

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

    val expected: Option[Yaml] = Some(Yaml.map(Map(
      Yaml.string("Mark McGwire") -> Yaml.map(Map(
        Yaml.string("hr") -> Yaml.int(65),
        Yaml.string("avg") -> Yaml.float(0.278)
      )),
      Yaml.string("Sammy Sosa") -> Yaml.map(Map(
        Yaml.string("hr") -> Yaml.int(63),
        Yaml.string("avg") -> Yaml.float(0.288)
      ))
    )))

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

    val expected: Option[Yaml] = Some(Yaml.stream(Stream(
      Yaml.seq(List(
        Yaml.string("Mark McGwire"),
        Yaml.string("Sammy Sosa"),
        Yaml.string("Ken Griffey")
      )),
      Yaml.seq(List(
        Yaml.string("Chicago Cubs"),
        Yaml.string("St Louis Cardinals")
      ))
    )))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  ignore should "correctly parse play by play feed from a game" in {
    // TODO: SnakeYaml interprets timestrings to seconds after midnight
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
      Yaml.map(Map(
        Yaml.string("time") -> Yaml.string("20:03:20"),
        Yaml.string("player") -> Yaml.string("Sammy Sosa"),
        Yaml.string("action") -> Yaml.string("strike (miss)")
      )),
      Yaml.map(Map(
        Yaml.string("time") -> Yaml.string("20:03:47"),
        Yaml.string("player") -> Yaml.string("Sammy Sosa"),
        Yaml.string("action") -> Yaml.string("grand slam")
      ))
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

    val expected: Option[Yaml] = Some(Yaml.map(Map(
      Yaml.string("hr") -> Yaml.seq(List(
        Yaml.string("Mark McGwire"),
        Yaml.string("Sammy Sosa")
      )),
      Yaml.string("rbi") -> Yaml.seq(List(
        Yaml.string("Sammy Sosa"),
        Yaml.string("Ken Griffey")
      ))
    )))

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

    val expected: Option[Yaml] = Some(Yaml.map(Map(
      Yaml.string("hr") -> Yaml.seq(List(
        Yaml.string("Mark McGwire"),
        Yaml.string("Sammy Sosa")
      )),
      Yaml.string("rbi") -> Yaml.seq(List(
        Yaml.string("Sammy Sosa"),
        Yaml.string("Ken Griffey")
      ))
    )))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  ignore should "correctly parse Mapping between Sequences" in {
    // TODO: SnakeYaml interprets datestrings as java datetimes
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

    val expected: Option[Yaml] = Some(Yaml.map(Map(
      Yaml.seq(List(
        Yaml.string("Detroit Tigers"),
        Yaml.string("Chicago Cubs")
      )) -> Yaml.seq(List(
        Yaml.string("2001-07-23")
      )),
      Yaml.seq(List(
        Yaml.string("New York Yankees"),
        Yaml.string("Atlanta Braves")
      )) -> Yaml.seq(List(
        Yaml.string("2001-07-02"),
        Yaml.string("2001-08-12"),
        Yaml.string("2001-08-14")
      ))
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

    val expected: Option[Yaml] = Some(Yaml.seq(List(
      Yaml.map(Map(
        Yaml.string("item") -> Yaml.string("Super Hoop"),
        Yaml.string("quantity") -> Yaml.int(1)
      )),
      Yaml.map(Map(
        Yaml.string("item") -> Yaml.string("Basketball"),
        Yaml.string("quantity") -> Yaml.int(4)
      )),
      Yaml.map(Map(
        Yaml.string("item") -> Yaml.string("Big Shoes"),
        Yaml.string("quantity") -> Yaml.int(1)
      ))
    )))

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

    val expected: Option[Yaml] = Some(Yaml.string("\\//||\\/||\n// ||  ||__\n"))

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

    val expected: Option[Yaml] = Some(Yaml.string(
      "Sammy Sosa completed another fine season with great stats.\n\n  63 Home Runs\n  0.288 Batting Average\n\nWhat a year!\n"
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

    val expected: Option[Yaml] = Some(Yaml.map(Map(
      Yaml.string("name") -> Yaml.string("Mark McGwire"),
      Yaml.string("accomplishment") -> Yaml.string("Mark set a major league home run record in 1998.\n"),
      Yaml.string("stats") -> Yaml.string("65 Home Runs\n0.278 Batting Average\n")
    )))

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

    val expected: Option[Yaml] = Some(Yaml.map(Map(
      Yaml.string("unicode") -> Yaml.string("Sosa did fine.\u263A"),
      Yaml.string("control") -> Yaml.string("\b1998\t1999\t2000\n"),
      Yaml.string("hex esc") -> Yaml.string("\r\n is \r\n"),
      Yaml.string("single") -> Yaml.string("\"Howdy!\" he cried."),
      Yaml.string("quoted") -> Yaml.string(" # Not a 'comment'."),
      Yaml.string("tie-fighter") -> Yaml.string("|\\-*-/|")
    )))

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

    val expected: Option[Yaml] = Some(Yaml.map(Map(
      Yaml.string("plain") -> Yaml.string("This unquoted scalar spans many lines."),
      Yaml.string("quoted") -> Yaml.string("So does this quoted scalar.\n")
    )))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual should be(expected)
  }

  behavior of "Yaml.print"

  it should "correctly print simple yaml sequences" in {
    // given
    val yaml: Yaml = Yaml.seq(List(
      Yaml.string("Mark McGwire"),
      Yaml.string("Sammy Sosa"),
      Yaml.string("Ken Griffey")
    ))

    val expected: String = "[Mark McGwire, Sammy Sosa, Ken Griffey]"

    // when
    val actual: String = yaml.print

    // then
    actual should be(expected)
  }

  it should "correctly print simple yaml mappings" in {
    // given
    val yaml: Yaml = Yaml.map(Map(
      Yaml.string("hr") -> Yaml.int(65),
      Yaml.string("avg") -> Yaml.float(0.278),
      Yaml.string("rbi") -> Yaml.int(147)
    ))

    val expected: String = "{hr: 65, avg: 0.278, rbi: 147}"

    // when
    val actual: String = yaml.print

    // then
    actual should be(expected)
  }

  it should "correctly print simple yaml streams" in {
    // given
    val yaml: Yaml = Yaml.stream(Stream(
      Yaml.map(Map(
        Yaml.string("time") -> Yaml.string("20:03:20"),
        Yaml.string("player") -> Yaml.string("Sammy Sosa"),
        Yaml.string("action") -> Yaml.string("strike (miss)")
      )),
      Yaml.map(Map(
        Yaml.string("time") -> Yaml.string("20:03:47"),
        Yaml.string("player") -> Yaml.string("Sammy Sosa"),
        Yaml.string("action") -> Yaml.string("grand slam")
      ))
    ))

    val expected: String =
      """{time: 20:03:20, player: Sammy Sosa, action: strike (miss)}
        |...
        |{time: 20:03:47, player: Sammy Sosa, action: grand slam}
        |...
        |""".stripMargin

    // when
    val actual: String = yaml.print

    // then
    actual should be(expected)
  }
}
