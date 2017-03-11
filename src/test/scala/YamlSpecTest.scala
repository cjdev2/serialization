import org.scalatest.{FlatSpec, Matchers}

class YamlSpecTest extends FlatSpec with Matchers {

  import com.cj.serialization.yaml.Yaml

  behavior of "Yaml.parse"

  it should "correctly parse simple yaml sequences" in {
    // given
    val raw: String =
      """- Mark McGwire
        |- Sammy Sosa
        |- Ken Griffey""".stripMargin

    val expected: Yaml = Yaml.seq(List(
      Yaml.string("Mark McGwire"),
      Yaml.string("Sammy Sosa"),
      Yaml.string("Ken Griffey")
    ))

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual.contains(expected) should be(true)
  }

  it should "correctly parse simple yaml mappings" in {
    // given
    val raw: String =
      """hr:  65    # Home runs
        |avg: 0.278 # Batting average
        |rbi: 147   # Runs Batted In""".stripMargin

    val expected: Yaml = Yaml(
      "hr" -> Yaml.int(65),
      "avg" -> Yaml.float(0.278),
      "rbi" -> Yaml.int(147)
    )

    // when
    val actual: Option[Yaml] = Yaml.parse(raw)

    // then
    actual.contains(expected) should be(true)
  }

  it should "correctly parse "

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
    val yaml: Yaml = Yaml(
      "hr" -> Yaml.int(65),
      "avg" -> Yaml.float(0.278),
      "rbi" -> Yaml.int(147)
    )

    val expected: String = "{hr: 65, avg: 0.278, rbi: 147}"

    // when
    val actual: String = yaml.print

    // then
    actual should be(expected)
  }
}
