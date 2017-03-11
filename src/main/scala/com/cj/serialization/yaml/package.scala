package com.cj.serialization

package object yaml {

  trait YamlCodec[T] extends Serializable[T] with Deserializable[T] {

    def toYaml(t: T): Yaml
    def fromYaml(yaml: Yaml): Option[T]

    final def toYamlString(t: T): String = ???
    final def toPrettyYamlString(t: T): String = ???
    final def fromYamlString(string: String): Option[T] = ???

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

  implicit def yamlCodecList[T](
                                 implicit
                                 codecT: YamlCodec[T]
                               ): YamlCodec[List[T]] =
    new YamlCodec[List[T]] {

      import scalaz._, Scalaz._

      def toYaml(t: List[T]): Yaml =
        Yaml.seq(t.map(codecT.toYaml))

      def fromYaml(yaml: Yaml): Option[List[T]] =
        yaml.asList.flatMap(_.map(codecT.fromYaml).sequence)
    }

//  implicit def yamlCodecMap[K, V](
//                                   implicit
//                                   codecK: YamlCodec[K],
//                                   codecV: YamlCodec[V]
//                                 ): YamlCodec[Map[K, V]] =
//    new YamlCodec[Map[K, V]] {
//
//      import scalaz._, Scalaz._
//
//      def toYaml(t: Map[K, V]): Yaml =
//        Yaml.map(t.map({ case (k, v) => (codecK.toYaml(k), codecV.toYaml(v)) }))
//
//      def fromYaml(yaml: Yaml): Option[Map[K, V]] = for {
//        m1 <- yaml.asMap
//        m2 <- m1.map({
//          case (yk, yv) => for {
//            k <- codecK.fromYaml(yk)
//            v <- codecV.fromYaml(yv)
//          } yield (k, v)
//        }).toList.sequence
//      } yield m2.toMap
//    }

//  implicit def yamlCodecSet[T](
//                                implicit
//                                codecT: YamlCodec[T]
//                              ): YamlCodec[Set[T]] =
//    new YamlCodec[Set[T]] {
//
//      import scalaz._, Scalaz._
//
//      def toYaml(t: Set[T]): Yaml =
//        Yaml.set(t.map(codecT.toYaml))
//
//      def fromYaml(yaml: Yaml): Option[Set[T]] =
//        yaml.asSet.flatMap(_.map(codecT.fromYaml).toList.sequence).map(_.toSet)
//    }

  class YamlCodecFromConverters[T](
                                    to: T => Yaml,
                                    from: Yaml => Option[T]
                                  ) extends YamlCodec[T] {
    def toYaml(t: T): Yaml = to(t)
    def fromYaml(yaml: Yaml): Option[T] = from(yaml)
  }
}
