package com.cj.serialization.yaml

sealed abstract class Yaml extends Product with Serializable {

  import YamlS._

  def fold[X](
               withScalar: String => X,
               withSeq: List[Yaml] => X,
               withMap: Map[Yaml, Yaml] => X,
               withStream: Stream[Yaml] => X
             ): X =
    this match {
      case YScalar(string) => withScalar(string)
      case YSeq(seq) => withSeq(seq)
      case YMap(map) => withMap(map)
      case YStream(stream) => withStream(stream)
    }

  def ~>(key: Yaml): Option[Yaml] = this match {
    case YMap(map) => map.get(key)
    case _ => None
  }

  def ~>(key: String): Option[Yaml] = this match {
    case YMap(map) => map.get(Yaml.string(key))
    case _ => None
  }

  def ~>(key: Int): Option[Yaml] = this match {
    case YMap(map) => map.get(Yaml.int(key))
    case YSeq(seq) => seq.lift(key)
    case _ => None
  }

  def asMap: Option[Map[Yaml, Yaml]] = this match {
    case YMap(map) => safely(map)
    case _ => None
  }

  def asList: Option[List[Yaml]] = this match {
    case YSeq(seq) => safely(seq)
    case _ => None
  }

  def asStream: Option[Stream[Yaml]] = this match {
    case YStream(stream) => safely(stream)
    case _ => None
  }

  def yNull: Option[Unit] = this match {
    case YScalar(s) if matchNull(s) => safely({})
    case _ => None
  }

  def yNan: Option[Unit] = this match {
    case YScalar(s) if matchNan(s) => safely({})
    case _ => None
  }

  def yInf: Option[Unit] = this match {
    case YScalar(s) if matchInf(s) => safely({})
    case _ => None
  }

  def yNegInf: Option[Unit] = this match {
    case YScalar(s) if matchNegInf(s) => safely({})
    case _ => None
  }

  def bool: Option[Boolean] = this match {
    case YScalar(s) if matchTrue(s) => safely(true)
    case YScalar(s) if matchFalse(s) => safely(false)
    case _ => None
  }

  def int: Option[Long] = this match {
    case YScalar(s) => safely(s.toLong)
    case _ => None
  }

  def float: Option[Double] = this match {
    case YScalar(s) => safely(s.toDouble)
    case _ => None
  }

  def string: Option[String] = this match {
    case YScalar(s) => safely(s)
    case _ => None
  }

  def print: String = printYaml(this)

  def prettyPrint: String = prettyPrintYaml(this)
}

object Yaml {

  def apply(n: Long): Yaml = Yaml.int(n)
  def apply(x: Double): Yaml = Yaml.float(x)
  def apply(p: Boolean): Yaml = Yaml.bool(p)
  def apply(s: String): Yaml = Yaml.string(s)
  def apply(pairs: (Yaml, Yaml)*): Yaml = Yaml.map(pairs.toMap)

  def parse(raw: String): Option[Yaml] = YamlS.parseYaml(raw)

  def yNull: Yaml = YScalar("~")
  def yNan: Yaml = YScalar(".nan")
  def yInf: Yaml = YScalar(".inf")
  def yNegInf: Yaml = YScalar("-.inf")
  def bool(p: Boolean): Yaml = YScalar(if (p) "true" else "false")
  def int(n: Long): Yaml = YScalar(n.toString)
  def float(x: Double): Yaml = YScalar(x.toString)
  def string(s: String): Yaml = YScalar(s)
  def seq(seq: List[Yaml]): Yaml = YSeq(seq)
  def map(map: Map[Yaml, Yaml]): Yaml = YMap(map)
  def stream(stream: Stream[Yaml]): Yaml = YStream(stream)

  implicit class YamlOp(x: Option[Yaml]) {
    def ~>(key: Yaml): Option[Yaml] = x.flatMap(y => y ~> key)
    def ~>(key: String): Option[Yaml] = x.flatMap(y => y ~> key)
    def ~>(key: Int): Option[Yaml] = x.flatMap(y => y ~> key)
    def asMap: Option[Map[Yaml, Yaml]] = x.flatMap(y => y.asMap)
    def asList: Option[List[Yaml]] = x.flatMap(y => y.asList)
    def asStream: Option[Stream[Yaml]] = x.flatMap(y => y.asStream)
    def yNull: Option[Unit] = x.flatMap(y => y.yNull)
    def yNan: Option[Unit] = x.flatMap(y => y.yNan)
    def yInf: Option[Unit] = x.flatMap(y => y.yInf)
    def yNegInf: Option[Unit] = x.flatMap(y => y.yNegInf)
    def bool: Option[Boolean] = x.flatMap(y => y.bool)
    def int: Option[Long] = x.flatMap(y => y.int)
    def float: Option[Double] = x.flatMap(y => y.float)
    def string: Option[String] = x.flatMap(y => y.string)
  }
}

private case class YScalar(scalar: String) extends Yaml
private case class YSeq(seq: List[Yaml]) extends Yaml
private case class YMap(map: Map[Yaml, Yaml]) extends Yaml
private case class YStream(stream: Stream[Yaml]) extends Yaml

private object YamlS {

  def safely[T](t: => T): Option[T] =
    scala.util.Try(t).toOption.flatMap(Option.apply)

  def parseYaml(raw: String): Option[Yaml] = {

    import org.yaml.snakeyaml.{Yaml => SnakeYaml}
    import scala.collection.JavaConversions._
    import scalaz._, Scalaz._

    def parseDoc(a: Any): Option[Yaml] = a match {
      case a: java.util.Map[_, _] => parseMap(a.toMap)
      case a: java.util.List[_] => parseList(a.toList)
      case _ => safely(YScalar(a.toString))
    }

    def parseMap(map: Map[Any, Any]): Option[Yaml] =
      map.toList.map(kv => for {
        k <- parseDoc(kv._1)
        v <- parseDoc(kv._2)
      } yield (k, v)).sequence.map(l => YMap(l.toMap))

    def parseList(list: List[_]): Option[Yaml] =
      list.map(a => parseDoc(a)).sequence.map(YSeq)

    def parseStream(stream: Stream[_]): Option[Yaml] =
      stream.map(a => parseDoc(a)).sequence.map(YStream)

    safely(new SnakeYaml().load(raw)) match {
      case Some(a) => parseDoc(a)
      case None => safely(new SnakeYaml().loadAll(raw)) match {
        case Some(it) => parseStream(it.toStream)
        case None => None
      }
    }
  }

  def printYaml(y: Yaml): String = y.fold(
    withScalar = escapeIf,
    withSeq = "[" + _.map(_.print).mkString(", ") + "]",
    withMap = {
      def f(kv: (Yaml, Yaml)): String = kv._1.print + ": " + kv._2.print
      "{" + _.map(f).mkString(", ") + "}"
    },
    withStream = _.foldLeft("")((s, y) => s + y.print + "\n...\n")
  )

  def prettyPrintYaml(y: Yaml): String = {

    //def makeLine(indentLevel: Int, contents: String): String =
    //  (1 to indentLevel).foldLeft("")((acc, _) => acc + "  ") + contents + "\n"

    // def helper(currentIndent: Int, acc: String, rest: Yaml): String = ???

    // helper(0, "", y)

    y.print
  }

  def escapeIf(raw: String): String = {
    val s = escape(raw)
    if (s.contains("\\")) s else raw
  }

  def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }

  def matchNull(s: String): Boolean = s match {
    case "~" => true
    case "null" => true
    case "Null" => true
    case "NULL" => true
    case _ => false
  }

  def matchNan(s: String): Boolean = s match {
    case ".nan" => true
    case ".Nan" => true
    case ".NaN" => true
    case ".NAN" => true
    case _ => false
  }

  def matchInf(s: String): Boolean = s match {
    case ".inf" => true
    case ".Inf" => true
    case ".INF" => true
    case _ => false
  }

  def matchNegInf(s: String): Boolean = s match {
    case "-.inf" => true
    case "-.Inf" => true
    case "-.INF" => true
    case _ => false
  }

  def matchTrue(s: String): Boolean = s match {
    case "t" => true
    case "T" => true
    case "true" => true
    case "True" => true
    case "TRUE" => true
    case "y" => true
    case "Y" => true
    case "yes" => true
    case "Yes" => true
    case "YES" => true
    case "on" => true
    case "On" => true
    case "ON" => true
    case _ => false
  }

  def matchFalse(s: String): Boolean = s match {
    case "f" => true
    case "F" => true
    case "false" => true
    case "False" => true
    case "FALSE" => true
    case "n" => true
    case "N" => true
    case "no" => true
    case "No" => true
    case "NO" => true
    case "off" => true
    case "Off" => true
    case "OFF" => true
    case _ => false
  }
}
