package com.cj.serialization

package object yaml {

  trait YamlCodec[T] extends Serializable[T] with Deserializable[T] {

    def toYaml(t: T): Yaml
    def fromYaml(yaml: Yaml): Option[T]

    final def toYamlString(t: T): String =  toYaml(t).print
    final def toPrettyYamlString(t: T): String = toYaml(t).prettyPrint(2)

    final def fromYamlString(string: String): Option[T] =
      Yaml.parse(string).flatMap(fromYaml)

    final def serialize(t: T): Array[Byte] =
      implicitly[Serializable[String]].serialize(toYamlString(t))

    final def deserialize(bytes: Array[Byte]): Option[T] = for {
      string <- implicitly[Deserializable[String]].deserialize(bytes)
      t <- fromYamlString(string)
    } yield t
  }

  def toYaml[T: YamlCodec](t: T): Yaml =
    implicitly[YamlCodec[T]].toYaml(t)

  def toYamlString[T: YamlCodec](t: T): String =
    implicitly[YamlCodec[T]].toYamlString(t)

  def toPrettyYamlString[T: YamlCodec](t: T): String =
    implicitly[YamlCodec[T]].toPrettyYamlString(t)

  def fromYaml[T: YamlCodec](yaml: Yaml): Option[T] =
    implicitly[YamlCodec[T]].fromYaml(yaml)

  def fromYamlString[T: YamlCodec](string: String): Option[T] =
    implicitly[YamlCodec[T]].fromYamlString(string)

  implicit object YamlCodecYaml extends YamlCodec[Yaml] {
    def toYaml(t: Yaml): Yaml = t
    def fromYaml(yaml: Yaml): Option[Yaml] = Option(yaml)
  }

  implicit object YamlCodecString extends YamlCodec[String] {
    def toYaml(t: String): Yaml = Yaml.string(t)
    def fromYaml(yaml: Yaml): Option[String] = yaml.string
  }

  implicit object YamlCodecLong extends YamlCodec[Long] {
    def toYaml(t: Long): Yaml = Yaml.long(t)
    def fromYaml(yaml: Yaml): Option[Long] = yaml.long
  }

  implicit object YamlCodecDouble extends YamlCodec[Double] {
    def toYaml(t: Double): Yaml = Yaml.double(t)
    def fromYaml(yaml: Yaml): Option[Double] = yaml.double
  }

  implicit object YamlCodecBoolean extends YamlCodec[Boolean] {
    def toYaml(t: Boolean): Yaml = Yaml.bool(t)
    def fromYaml(yaml: Yaml): Option[Boolean] = yaml.bool
  }

  implicit def yamlCodecList[T](
                                 implicit
                                 ev: YamlCodec[T]
                               ): YamlCodec[List[T]] =
    new YamlCodec[List[T]] {
      import scalaz._, Scalaz._

      def toYaml(t: List[T]): Yaml =
        Yaml.array(t.map(ev.toYaml))

      def fromYaml(yaml: Yaml): Option[List[T]] =
        yaml.array.flatMap(_.map(ev.fromYaml).sequence)
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

      def fromYaml(yaml: Yaml): Option[Map[K, V]] =
        yaml.assoc.flatMap(
          _.toList.map(kv => for {
            k <- ev1.fromYaml(kv._1)
            v <- ev2.fromYaml(kv._2)
          } yield (k, v)).sequence.map(_.toMap)
        )
    }

  implicit def yamlCodecStream[T](
                                   implicit
                                   ev: YamlCodec[T]
                                 ): YamlCodec[Stream[T]] =
    new YamlCodec[Stream[T]] {
      import scalaz._, Scalaz._

      def toYaml(t: Stream[T]): Yaml =
        Yaml.stream(t.map(ev.toYaml))

      def fromYaml(yaml: Yaml): Option[Stream[T]] =
        yaml.stream.flatMap(_.map(ev.fromYaml).sequence)
    }
}
