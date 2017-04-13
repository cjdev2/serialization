import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class YamlTest extends FlatSpec with Matchers with PropertyChecks {

  import com.cj.serialization.Result
  import com.cj.serialization.yaml._
  import com.cj.serialization.json.Json
  import Fixtures._

  behavior of "YamlCodecYaml"

  it should "satisfy the first serialization law" in {
    type T = Yaml
    val gen = implicitly[Gen[T]]
    forAll(gen) {
      // given
      (t: T) =>
        val expected = Result.safely(t)

        // when
        val actual = fromYaml[T](toYaml[T](t))

        // then
        actual should be(expected)
    }
  }

  it should "satisfy the second serialization law" in {
    val gen = implicitly[Gen[Yaml]]
    type T = Yaml
    forAll(gen) {
      // given
      (yaml: Yaml) =>
        val expected = fromYaml[T](yaml)

        // when
        val actual = fromYaml[T](yaml).map(toYaml[T]).flatMap(fromYaml[T])

        // then
        actual should be(expected)
    }
  }

  it should "satisfy the serialization laws non-trivially" in {
    // given
    val yaml: Yaml = Yaml.assoc(Map(Yaml.string("foo") -> Yaml.string("bar")))

    // when
    val result1: Yaml =
      fromYaml[Yaml](toYaml[Yaml](yaml)).getOrThrow

    val result2: Yaml =
      fromYaml[Yaml](yaml).map(toYaml[Yaml]).flatMap(fromYaml[Yaml]).getOrThrow

    // then
    result1 should be(yaml)
    result2 should be(yaml)
  }

  behavior of "YamlCodecJson"

  // TODO: Fix Yaml AST representation to allow lossless JSON conversion
  ignore should "satisfy the first serialization law" in {
    type T = Json
    val gen = implicitly[Gen[T]]
    forAll(gen) {
      // given
      (t: T) =>
        val expected = Result.safely(t)

        // when
        val actual = fromYaml[T](toYaml[T](t))

        // then
        actual should be(expected)
    }
  }

  it should "satisfy the second serialization law" in {
    val gen = implicitly[Gen[Yaml]]
    type T = Json
    forAll(gen) {
      // given
      (yaml: Yaml) =>
        val expected = fromYaml[T](yaml)

        // when
        val actual = fromYaml[T](yaml).map(toYaml[T]).flatMap(fromYaml[T])

        // then
        actual should be(expected)
    }
  }

  it should "satisfy the serialization laws non-trivially" in {
    // given
    val json: Json = Json.assoc(Map("foo" -> Json.string("bar")))
    val yaml: Yaml = Yaml.assoc(Map(Yaml.string("foo") -> Yaml.string("bar")))

    // when
    val result1: Json =
      fromYaml[Json](toYaml[Json](json)).getOrThrow

    val result2: Json =
      fromYaml[Json](yaml).map(toYaml[Json]).flatMap(fromYaml[Json]).getOrThrow

    // then
    result1 should be(json)
    result2 should be(json)
  }

  object Fixtures {

    type Gen[T] = org.scalacheck.Gen[T]

    import org.scalacheck.Gen
    import org.scalacheck.Arbitrary

    implicit lazy val genYaml: Gen[Yaml] = {

      lazy val genYScalar =
        Arbitrary.arbitrary[String].map(Yaml.scalar)

      lazy val genYSequence =
        Gen.listOf(genYScalar).map(Yaml.array)

      lazy val genYMapping = {

        lazy val genPair = for {
          key <- genYScalar
          value <- genYScalar
        } yield (key, value)

        Gen.listOf(genPair).map(x => Yaml.assoc(x.toMap))
      }

      lazy val genFlatYaml =
        Gen.oneOf(genYSequence, genYMapping)

      lazy val genNestedYSequence =
        Gen.listOf(genFlatYaml).map(Yaml.array)

      lazy val genNestedYMapping = {

        lazy val genPair = for {
          key <- genFlatYaml
          value <- genFlatYaml
        } yield (key, value)

        Gen.listOf(genPair).map(x => Yaml.assoc(x.toMap))
      }

      lazy val genNestedYaml =
        Gen.oneOf(genNestedYSequence, genNestedYMapping)

      Gen.oneOf(genFlatYaml, genNestedYaml)
    }

    implicit lazy val genJson: Gen[Json] = {

      lazy val genValue = {

        lazy val genNum =
          Gen.oneOf(
            Arbitrary.arbitrary[Long].map(Json.long),
            Arbitrary.arbitrary[Double].map(Json.double)
          )

        lazy val genString =
          Arbitrary.arbitrary[String].map(Json.string)

        lazy val genBool =
          Arbitrary.arbitrary[Boolean].map(Json.bool)

        lazy val genNull =
          Gen.const(Json.nul)

        Gen.oneOf(genNum, genString, genBool, genNull)
      }

      lazy val genObject = {

        lazy val genPair = for {
          key <- Arbitrary.arbitrary[String]
          value <- genValue
        } yield (key, value)

        Gen.listOf(genPair).map(pairs => Json.assoc(pairs.toMap))
      }

      lazy val genArray =
        Gen.listOf(genValue).map(Json.array)

      lazy val genFlatJson =
        Gen.oneOf(genArray, genObject)

      lazy val genNestedArray =
        Gen.listOf(genFlatJson).map(Json.array)

      lazy val genNestedObject = {

        lazy val genPair = for {
          key <- Arbitrary.arbitrary[String]
          value <- genFlatJson
        } yield (key, value)

        Gen.listOf(genPair).map(pairs => Json.assoc(pairs.toMap))
      }

      val genNestedJson =
        Gen.oneOf(genNestedArray, genNestedObject)

      Gen.oneOf(genFlatJson, genNestedJson)
    }
  }
}
