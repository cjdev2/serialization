package com.cj.serialization

package object yaml {

  import com.cj.serialization.json.Json

  trait YamlCodec[T] extends Serialize[T] with Deserialize[T] {

    def toYaml(t: T): Yaml
    def fromYaml(yaml: Yaml): Result[T]

    final def printYaml(t: T): String =  toYaml(t).print
    final def prettyYaml(t: T): String = toYaml(t).prettyPrint(2)

    final def parseYaml(string: String): Result[T] =
      Yaml.parse(string).flatMap(fromYaml)

    final def serialize(t: T): Array[Byte] =
      implicitly[Serialize[String]].serialize(printYaml(t))

    final def deserialize(bytes: Array[Byte]): Result[T] = for {
      string <- implicitly[Deserialize[String]].deserialize(bytes)
      t <- parseYaml(string)
    } yield t
  }

  def toYaml[T: YamlCodec](t: T): Yaml =
    implicitly[YamlCodec[T]].toYaml(t)

  def printYaml[T: YamlCodec](t: T): String =
    implicitly[YamlCodec[T]].printYaml(t)

  def prettyYaml[T: YamlCodec](t: T): String =
    implicitly[YamlCodec[T]].prettyYaml(t)

  def fromYaml[T: YamlCodec](yaml: Yaml): Result[T] =
    implicitly[YamlCodec[T]].fromYaml(yaml)

  def parseYaml[T: YamlCodec](string: String): Result[T] =
    implicitly[YamlCodec[T]].parseYaml(string)

  object YamlCodec {

    def apply[T](to: T => Yaml, from: Yaml => Result[T]): YamlCodec[T] =
      new YamlCodec[T] {
        def toYaml(t: T): Yaml = to(t)
        def fromYaml(yaml: Yaml): Result[T] = from(yaml)
      }

    implicit object YamlCodecYaml extends YamlCodec[Yaml] {
      def toYaml(t: Yaml): Yaml = t

      def fromYaml(yaml: Yaml): Result[Yaml] = Result.safely(yaml)
    }

    implicit object YamlCodecJson extends YamlCodec[Json] {

      // This transformation is lossy
      // e.g., imagine you have json `j = {"foo": "0"}`
      // then toYaml(j) = foo: 0
      // and then fromYaml(toYaml(j)) = Some({"foo": 0}) =/ Some(j)
      // TODO: implement a YAML ADT that allows for a lossless JSON translation

      import scalaz._, Scalaz._

      def toYaml(t: Json): Yaml =
        t.fold(
          withNull = Yaml.scalar("~"),
          withBoolean = p => Yaml.scalar(p.toString),
          withNumber = n => Yaml.scalar(n.toString),
          withString = s => Yaml.scalar(s),
          withArray = js => Yaml.array(js.map(toYaml)),
          withAssoc = obj => {
            Yaml.assoc(obj.toList.map({
              case (key, value) =>
                (Yaml.scalar(key), toYaml(value))
            }).toMap)
          }
        )

      def fromYaml(yaml: Yaml): Result[Json] =
        yaml.fold[Result[Json]](
          withScalar = {
            case _ if yaml.nul.isDefined =>
              Result.safely(Json.nul)
            case _ if yaml.nan.isDefined =>
              Result.safely(Json.string("NaN"))
            case _ if yaml.inf.isDefined =>
              Result.safely(Json.string("Infinity"))
            case _ if yaml.neginf.isDefined =>
              Result.safely(Json.string("-Infinity"))
            case _ if yaml.bool.isDefined =>
              Result.safely(Json.bool(yaml.bool.get))
            case _ if yaml.long.isDefined =>
              Result.safely(Json.number(yaml.long.get))
            case _ if yaml.double.isDefined =>
              Result.safely(Json.number(yaml.double.get))
            case _ if yaml.string.isDefined =>
              Result.safely(Json.string(yaml.string.get))
            case _ =>
              Result.failure(s"YamlCodecJson.fromYaml: unexpected error, ${yaml.scalar.get}")
          },
          withSeq = array => array.map(fromYaml).sequence.map(Json.array),
          withMap = assoc => {
            assoc.map({ case (ykey, yval) => ykey.scalar match {
              case None =>
                Result.failure(s"YamlCodecJson.fromYaml: Yaml key was not a scalar, $ykey")
              case Some(raw) => (raw, fromYaml(yval)).sequence
            }
            }).toList.sequence.map(pairs => Json.assoc(pairs.toMap))
          },
          withStream = _ =>
            Result.failure("YamlCodecJson.fromYaml: Yaml streams cannot be converted to Json")
        )
    }

    implicit object YamlCodecString extends YamlCodec[String] {
      def toYaml(t: String): Yaml = Yaml.string(t)

      def fromYaml(yaml: Yaml): Result[String] = yaml.string match {
        case None => Result.failure("YamlCodecString: source yaml could not be read as String")
        case Some(x) => Result.safely(x)
      }
    }

    implicit object YamlCodecLong extends YamlCodec[Long] {
      def toYaml(t: Long): Yaml = Yaml.long(t)

      def fromYaml(yaml: Yaml): Result[Long] = yaml.long match {
        case None => Result.failure("YamlCodecLong: source yaml could not be read as Long")
        case Some(x) => Result.safely(x)
      }
    }

    implicit object YamlCodecDouble extends YamlCodec[Double] {
      def toYaml(t: Double): Yaml = Yaml.double(t)

      def fromYaml(yaml: Yaml): Result[Double] = yaml.double match {
        case None => Result.failure("YamlCodecDouble: source yaml could not be read as Double")
        case Some(x) => Result.safely(x)
      }
    }

    implicit object YamlCodecBoolean extends YamlCodec[Boolean] {
      def toYaml(t: Boolean): Yaml = Yaml.bool(t)

      def fromYaml(yaml: Yaml): Result[Boolean] = yaml.bool match {
        case None => Result.failure("YamlCodecBoolean: source yaml could not be reas as Boolean")
        case Some(x) => Result.safely(x)
      }
    }

    implicit def yamlCodecList[T](
                                   implicit
                                   ev: YamlCodec[T]
                                 ): YamlCodec[List[T]] =
      new YamlCodec[List[T]] {

        import scalaz._, Scalaz._

        def toYaml(t: List[T]): Yaml =
          Yaml.array(t.map(ev.toYaml))

        def fromYaml(yaml: Yaml): Result[List[T]] =
          yaml.array match {
            case None =>
              Result.failure("yamlCodecList: source yaml was not a sequence")
            case Some(ys) =>
              ys.map(ev.fromYaml).sequence
          }
      }

    implicit def yamlCodecMap[K, V](
                                     implicit
                                     ev1: YamlCodec[K],
                                     ev2: YamlCodec[V]
                                   ): YamlCodec[Map[K, V]] =
      new YamlCodec[Map[K, V]] {

        import scalaz._, Scalaz._

        def toYaml(t: Map[K, V]): Yaml =
          Yaml.assoc(t.map(kv => (ev1.toYaml(kv._1), ev2.toYaml(kv._2))))

        def fromYaml(yaml: Yaml): Result[Map[K, V]] =
          yaml.assoc match {
            case None =>
              Result.failure("yamlCodecMap: source yaml was not a mapping")
            case Some(map) =>
              map.toList.map(kv => for {
                k <- ev1.fromYaml(kv._1)
                v <- ev2.fromYaml(kv._2)
              } yield (k, v)).sequence.map(_.toMap)
          }
      }

    implicit def yamlCodecStream[T](
                                     implicit
                                     ev: YamlCodec[T]
                                   ): YamlCodec[Stream[T]] =
      new YamlCodec[Stream[T]] {

        import scalaz._, Scalaz._

        def toYaml(t: Stream[T]): Yaml =
          Yaml.stream(t.map(ev.toYaml))

        def fromYaml(yaml: Yaml): Result[Stream[T]] =
          yaml.stream match {
            case None =>
              Result.failure("yamlCodecStream: source yaml was not a stream")
            case Some(stream) =>
              stream.map(ev.fromYaml).sequence
          }
      }
  }
}
