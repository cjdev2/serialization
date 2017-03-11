package com.cj.serialization.yaml

sealed abstract class Yaml extends Product with Serializable {

  import YamlS._

  def fold[X](
               withScalar: String => X,
               withSeq: List[Yaml] => X,
               withMap: Map[String, Yaml] => X
             ): X =
    this match {
      case YScalar(string) => withScalar(string)
      case YSeq(seq) => withSeq(seq)
      case YMap(map) => withMap(map)
    }

  def ~>(key: String): Option[Yaml] = this match {
    case YMap(map) => map.get(key)
    case _ => None
  }

  def ~>(n: Int): Option[Yaml] = this match {
    case YSeq(seq) => seq.lift(n)
    case YMap(map) => map.get(n.toString)
    case _ => None
  }

  def asMap: Option[Map[String, Yaml]] = this match {
    case YMap(map) => Option(map)
    case _ => None
  }

  def asList: Option[List[Yaml]] = this match {
    case YSeq(seq) => Option(seq)
    case _ => None
  }

  def yNull: Option[Unit] = this match {
    case YScalar(s) if matchNull(s) => Option({})
    case _ => None
  }

  def yNan: Option[Unit] = this match {
    case YScalar(s) if matchNan(s) => Option({})
    case _ => None
  }

  def yInf: Option[Unit] = this match {
    case YScalar(s) if matchInf(s) => Option({})
    case _ => None
  }

  def yNegInf: Option[Unit] = this match {
    case YScalar(s) if matchNegInf(s) => Option({})
    case _ => None
  }

  def bool: Option[Boolean] = this match {
    case YScalar(s) if matchTrue(s) => Option(true)
    case YScalar(s) if matchFalse(s) => Option(false)
    case _ => None
  }

  def long: Option[Long] = this match {
    case YScalar(s) => safely(s.toLong)
    case _ => None
  }

  def double: Option[Double] = this match {
    case YScalar(s) => safely(s.toDouble)
    case _ => None
  }

  def string: Option[String] = this match {
    case YScalar(s) => Option(stripQuotes(s))
    case _ => None
  }

  def print: String = printYaml(this)

  def prettyPrint: String = prettyPrintYaml(this)
}

object Yaml {

  def parse(raw: String): Option[Yaml] = YamlS.parseYaml(raw)

  def apply(fields: (String, Yaml)*): Yaml = YMap(fields.toMap)

  def yNull: Yaml = YScalar("~")
  def yNan: Yaml = YScalar(".nan")
  def yInf: Yaml = YScalar(".inf")
  def yNegInf: Yaml = YScalar("-.inf")
  def bool(p: Boolean): Yaml = YScalar(if (p) "true" else "false")
  def int(n: Long): Yaml = YScalar(n.toString)
  def float(x: Double): Yaml = YScalar(x.toString)
  def string(s: String): Yaml = YScalar(s)
  def seq(seq: List[Yaml]): Yaml = YSeq(seq)
  def map(map: Map[String, Yaml]): Yaml = YMap(map)

  implicit class DYamlOp(x: Option[Yaml]) {
    def ~>(key: String): Option[Yaml] = x.flatMap(y => y ~> key)
    def ~>(n: Int): Option[Yaml] = x.flatMap(y => y ~> n)
    def asMap: Option[Map[String, Yaml]] = x.flatMap(y => y.asMap)
    def asList: Option[List[Yaml]] = x.flatMap(y => y.asList)
    def nullLit: Option[Unit] = x.flatMap(y => y.yNull)
    def nanLit: Option[Unit] = x.flatMap(y => y.yNan)
    def infLit: Option[Unit] = x.flatMap(y => y.yInf)
    def negInfLit: Option[Unit] = x.flatMap(y => y.yNegInf)
    def bool: Option[Boolean] = x.flatMap(y => y.bool)
    def long: Option[Long] = x.flatMap(y => y.long)
    def double: Option[Double] = x.flatMap(y => y.double)
    def string: Option[String] = x.flatMap(y => y.string)
  }
}

private case class YScalar(scalar: String) extends Yaml
private case class YSeq(seq: List[Yaml]) extends Yaml
private case class YMap(map: Map[String, Yaml]) extends Yaml

private object YamlS {

  import org.yaml.snakeyaml.{Yaml => SnakeYaml}
  import scala.collection.JavaConversions._
  import scalaz._, Scalaz._

  def safely[T](t: => T): Option[T] =
    scala.util.Try(t).toOption.flatMap(Option.apply)

  def parseYaml(raw: String): Option[Yaml] = {

    val maybeMapping: Option[Map[String, Any]] = safely({
      val snakeYaml = new SnakeYaml().load(raw)
      snakeYaml.asInstanceOf[java.util.Map[String, Any]].toMap
    })

    val maybeSequence: Option[List[Any]] = safely({
      val snakeYaml = new SnakeYaml().load(raw)
      snakeYaml.asInstanceOf[java.util.ArrayList[Any]].toList
    })

    (maybeMapping, maybeSequence) match {
      case (Some(_), Some(_)) => None
      case (Some(map), None) => parseMap(map)
      case (None, Some(list)) => parseList(list)
      case (None, None) => Option(YScalar(raw))
    }
  }

  def parseMap(map: Map[String, Any]): Option[Yaml] =
    map.map({ case (s, a) => (s, parseYaml(a.toString)) }).sequence.map(YMap)

  def parseList(list: List[Any]): Option[Yaml] =
    list.map(a => parseYaml(a.toString)).sequence.map(YSeq)

  def printYaml(y: Yaml): String = y.fold(
    withScalar = identity,
    withSeq = "[" + _.map(_.print).mkString(", ") + "]",
    withMap = {
      def f(kv: (String, Yaml)): String = kv._1 + ": " + kv._2.print
      "{" + _.map(f).mkString(", ") + "}"
    }
  )

  def prettyPrintYaml(y: Yaml): String = {
//
//    def makeLine(indentLevel: Int, contents: String): String =
//      (1 to indentLevel).foldLeft("")((acc, _) => acc + "  ") + contents + "\n"
//
//    def helper(currentIndent: Int, acc: String, rest: Yaml): String = ???
//
//    helper(0, "", y)
    printYaml(y) // TODO: make pretty
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

  def stripQuotes(s: String): String = s match {
    case _ if s.nonEmpty && s.head == ''' && s.last == ''' =>
      s.slice(1, s.length - 1)
    case _ if s.nonEmpty && s.head == '"' && s.last == '"' =>
      s.slice(1, s.length - 1)
    case _ => s
  }
}
