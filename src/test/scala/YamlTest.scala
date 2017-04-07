import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class YamlTest extends FlatSpec with Matchers with PropertyChecks {

  import com.cj.serialization.yaml._
  import Fixtures._

  behavior of "YamlCodecYaml"

  it should "satisfy the first serialization law" in {
    type T = Yaml
    val gen = implicitly[Gen[T]]
    forAll(gen) {
      // given
      (t: T) =>
        val expected = Some(t)

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

  behavior of "YamlCodecJson"

  import com.cj.serialization.json.Json

  // TODO: It seems the Yaml spec is inconsistent...
  // TODO: I don't know if this test is fixable.
  ignore should "satisfy the first serialization law" in {
    type T = Json
    val gen = implicitly[Gen[T]]
    forAll(gen) {
      // given
      (t: T) =>
        val expected = Some(t)

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

//  behavior of "YamlCodecString"
//
//  it should "satisfy the first law" in {
//    type T = String
//
//    forAll { (t: T) =>
//
//      fromYaml[T](toYaml[T](t)) should be(Some(t))
//    }
//  }
//
//  it should "satisfy the second law" in {
//    type T = String
//
//    forAll { (yaml: Yaml) =>
//      fromYaml[T](yaml).map(toYaml[T]).flatMap(fromYaml[T]) should be(fromYaml[T])
//    }
//  }
//
//  behavior of "YamlCodecLong"
//
//  it should "satisfy the first law" in {
//    type T = Long
//
//    forAll { (t: T) =>
//
//      fromYaml[T](toYaml[T](t)) should be(Some(t))
//    }
//  }
//
//  it should "satisfy the second law" in {
//    type T = Long
//
//    forAll { (yaml: Yaml) =>
//      fromYaml[T](yaml).map(toYaml[T]).flatMap(fromYaml[T]) should be(fromYaml[T])
//    }
//  }
//
//  behavior of "YamlCodecDouble"
//
//  it should "satisfy the first law" in {
//    type T = Double
//
//    forAll { (t: T) =>
//
//      fromYaml[T](toYaml[T](t)) should be(Some(t))
//    }
//  }
//
//  it should "satisfy the second law" in {
//    type T = Double
//
//    forAll { (yaml: Yaml) =>
//      fromYaml[T](yaml).map(toYaml[T]).flatMap(fromYaml[T]) should be(fromYaml[T])
//    }
//  }
//
//  behavior of "YamlCodecBoolean"
//
//  it should "satisfy the first law" in {
//    type T = Boolean
//
//    forAll { (t: T) =>
//
//      fromYaml[T](toYaml[T](t)) should be(Some(t))
//    }
//  }
//
//  it should "satisfy the second law" in {
//    type T = Boolean
//
//    forAll { (yaml: Yaml) =>
//      fromYaml[T](yaml).map(toYaml[T]).flatMap(fromYaml[T]) should be(fromYaml[T])
//    }
//  }
//
//  behavior of "yamlCodecList"
//
//  it should "satisfy the first law" in {
//    type T = List[Yaml]
//
//    forAll { (t: T) =>
//
//      fromYaml[T](toYaml[T](t)) should be(Some(t))
//    }
//  }
//
//  it should "satisfy the second law" in {
//    type T = List[Yaml]
//
//    forAll { (yaml: Yaml) =>
//      fromYaml[T](yaml).map(toYaml[T]).flatMap(fromYaml[T]) should be(fromYaml[T])
//    }
//  }
//
//  behavior of "yamlCodecMap"
//
//  it should "satisfy the first law" in {
//    type T = Map[Yaml, Yaml]
//
//    forAll { (t: T) =>
//
//      fromYaml[T](toYaml[T](t)) should be(Some(t))
//    }
//  }
//
//  it should "satisfy the second law" in {
//    type T = Map[Yaml, Yaml]
//
//    forAll { (yaml: Yaml) =>
//      fromYaml[T](yaml).map(toYaml[T]).flatMap(fromYaml[T]) should be(fromYaml[T])
//    }
//  }
//
//  behavior of "yamlCodecStream"
//
//  it should "satisfy the first law" in {
//    type T = Stream[Yaml]
//
//    forAll { (t: T) =>
//
//      fromYaml[T](toYaml[T](t)) should be(Some(t))
//    }
//  }
//
//  it should "satisfy the second law" in {
//    type T = Stream[Yaml]
//
//    forAll { (yaml: Yaml) =>
//      fromYaml[T](yaml).map(toYaml[T]).flatMap(fromYaml[T]) should be(fromYaml[T])
//    }
//  }

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

      import argonaut.Json

      lazy val genValue = {

        lazy val genNum =
          Gen.oneOf(
            Arbitrary.arbitrary[Long].map(Json.jNumber),
            Arbitrary.arbitrary[Double].map(Json.jNumber).map(_.getOrElse(Json.jNull))
          )

        lazy val genString =
          Arbitrary.arbitrary[String].map(Json.jString)

        lazy val genBool =
          Arbitrary.arbitrary[Boolean].map(Json.jBool)

        lazy val genNull =
          Gen.const(Json.jNull)

        Gen.oneOf(genNum, genString, genBool, genNull)
      }

      lazy val genObject = {

        lazy val genPair = for {
          key <- Arbitrary.arbitrary[String]
          value <- genValue
        } yield (key, value)

        Gen.listOf(genPair).map(Json.jObjectAssocList)
      }

      lazy val genArray =
        Gen.listOf(genValue).map(Json.jArray)

      lazy val genFlatJson =
        Gen.oneOf(genArray, genObject)

      lazy val genNestedArray =
        Gen.listOf(genFlatJson).map(Json.jArray)

      lazy val genNestedObject = {

        lazy val genPair = for {
          key <- Arbitrary.arbitrary[String]
          value <- genFlatJson
        } yield (key, value)

        Gen.listOf(genPair).map(Json.jObjectAssocList)
      }

      val genNestedJson =
        Gen.oneOf(genNestedArray, genNestedObject)

      Gen.oneOf(genFlatJson, genNestedJson)
    }
  }
}
