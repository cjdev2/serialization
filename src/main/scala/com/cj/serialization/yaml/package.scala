package com.cj.serialization

package object yaml {

  import com.cj.serialization.json.Json
  import traversals._

  trait YamlCodec[T] extends Serialize[T] with Deserialize[T] {

    def toYaml(t: T): Yaml
    def fromYaml(yaml: Yaml): Option[T]

    final def printYaml(t: T): String =  toYaml(t).print
    final def prettyYaml(t: T): String = toYaml(t).prettyPrint(2)

    final def parseYaml(string: String): Option[T] =
      Yaml.parse(string).flatMap(fromYaml)

    final def serialize(t: T): Array[Byte] =
      implicitly[Serialize[String]].serialize(printYaml(t))

    final def deserialize(bytes: Array[Byte]): Option[T] = for {
      string <- implicitly[Deserialize[String]].deserialize(bytes)
      t <- parseYaml(string)
    } yield t
  }

  implicit object YamlCodecYaml extends YamlCodec[Yaml] {
    def toYaml(t: Yaml): Yaml = t
    def fromYaml(yaml: Yaml): Option[Yaml] = safely(yaml)
  }

  def toYaml[T: YamlCodec](t: T): Yaml =
    implicitly[YamlCodec[T]].toYaml(t)

  def printYaml[T: YamlCodec](t: T): String =
    implicitly[YamlCodec[T]].printYaml(t)

  def prettyYaml[T: YamlCodec](t: T): String =
    implicitly[YamlCodec[T]].prettyYaml(t)

  def fromYaml[T: YamlCodec](yaml: Yaml): Option[T] =
    implicitly[YamlCodec[T]].fromYaml(yaml)

  def parseYaml[T: YamlCodec](string: String): Option[T] =
    implicitly[YamlCodec[T]].parseYaml(string)

  object YamlCodec {

    def apply[T](to: T => Yaml, from: Yaml => Option[T]): YamlCodec[T] =
      new YamlCodec[T] {
        def toYaml(t: T): Yaml = to(t)
        def fromYaml(yaml: Yaml): Option[T] = from(yaml)
      }

    implicit object YamlCodecJson extends YamlCodec[Json] {

      // This transformation is lossy
      // e.g., imagine you have json `j = {"foo": "0"}`
      // then toYaml(j) = foo: 0
      // and then fromYaml(toYaml(j)) = Some({"foo": 0}) != Some(j)
      // TODO: implement a YAML ADT that allows for a lossless JSON translation

      def toYaml(t: Json): Yaml =
        t.fold[Yaml](
          withNull = Yaml.scalar("~"),
          withBoolean = p => Yaml.scalar(p.toString),
          withNumber = n => Yaml.scalar(n.toString),
          withString = s => Yaml.scalar(s),
          withArray = ys => Yaml.array(ys),
          withAssoc = msy =>
            Yaml.assoc(msy map { case (k, v) => (Yaml.scalar(k), v) })
        )

      def fromYaml(yaml: Yaml): Option[Json] =
        yaml.fold[Option[Json]](
          withScalar = {
            case _ if yaml.nul.isDefined =>
              safely(Json.nul)
            case _ if yaml.nan.isDefined =>
              safely(Json.string("NaN"))
            case _ if yaml.inf.isDefined =>
              safely(Json.string("Infinity"))
            case _ if yaml.neginf.isDefined =>
              safely(Json.string("-Infinity"))
            case _ if yaml.bool.isDefined =>
              safely(Json.bool(yaml.bool.get))
            case _ if yaml.long.isDefined =>
              safely(Json.number(yaml.long.get))
            case _ if yaml.double.isDefined =>
              safely(Json.number(yaml.double.get))
            case _ if yaml.string.isDefined =>
              safely(Json.string(yaml.string.get))
            case _ =>
              None
          },
          withSeq = array => array.traverse(fromYaml).map(Json.array),
          withMap = assoc => {
            assoc.toList.traverse({ case (ykey, yval) => ykey.scalar match {
              case None => None
              case Some(raw) => (raw, fromYaml(yval)).sequence
            }}).map(pairs => Json.assoc(pairs.toMap))
          },
          withStream = _ => None
        )
    }

    implicit object YamlCodecString extends YamlCodec[String] {
      def toYaml(t: String): Yaml = Yaml.string(t)

      def fromYaml(yaml: Yaml): Option[String] = yaml.string match {
        case None => None
        case Some(x) => safely(x)
      }
    }

    implicit object YamlCodecLong extends YamlCodec[Long] {
      def toYaml(t: Long): Yaml = Yaml.long(t)

      def fromYaml(yaml: Yaml): Option[Long] = yaml.long match {
        case None => None
        case Some(x) => safely(x)
      }
    }

    implicit object YamlCodecDouble extends YamlCodec[Double] {
      def toYaml(t: Double): Yaml = Yaml.double(t)

      def fromYaml(yaml: Yaml): Option[Double] = yaml.double match {
        case None => None
        case Some(x) => safely(x)
      }
    }

    implicit object YamlCodecBoolean extends YamlCodec[Boolean] {
      def toYaml(t: Boolean): Yaml = Yaml.bool(t)

      def fromYaml(yaml: Yaml): Option[Boolean] = yaml.bool match {
        case None => None
        case Some(x) => safely(x)
      }
    }

    implicit def yamlCodecList[T](
                                   implicit
                                   ev: YamlCodec[T]
                                 ): YamlCodec[List[T]] =
      new YamlCodec[List[T]] {

        def toYaml(t: List[T]): Yaml =
          Yaml.array(t.map(ev.toYaml))

        def fromYaml(yaml: Yaml): Option[List[T]] =
          yaml.array match {
            case None => None
            case Some(ys) => ys.traverse(ev.fromYaml)
          }
      }

    implicit def yamlCodecMap[K, V](
                                     implicit
                                     ev1: YamlCodec[K],
                                     ev2: YamlCodec[V]
                                   ): YamlCodec[Map[K, V]] =
      new YamlCodec[Map[K, V]] {

        def toYaml(t: Map[K, V]): Yaml =
          Yaml.assoc(t.map(kv => (ev1.toYaml(kv._1), ev2.toYaml(kv._2))))

        def fromYaml(yaml: Yaml): Option[Map[K, V]] =
          yaml.assoc match {
            case None => None
            case Some(map) =>
              map.toList.traverse(kv => for {
                k <- ev1.fromYaml(kv._1)
                v <- ev2.fromYaml(kv._2)
              } yield (k, v)).map(_.toMap)
          }
      }

    implicit def yamlCodecStream[T](
                                     implicit
                                     ev: YamlCodec[T]
                                   ): YamlCodec[Stream[T]] =
      new YamlCodec[Stream[T]] {

        def toYaml(t: Stream[T]): Yaml =
          Yaml.stream(t.map(ev.toYaml))

        def fromYaml(yaml: Yaml): Option[Stream[T]] =
          yaml.stream match {
            case None => None
            case Some(stream) => stream.traverse(ev.fromYaml)
          }
      }
  }
}
