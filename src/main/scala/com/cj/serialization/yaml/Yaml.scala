package com.cj.serialization
package yaml

import traversals._

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
    case YMap(map) => map.get(Yaml.long(key))
    case YSeq(seq) => seq.lift(key)
    case _ => None
  }

  def ><(f: Yaml => Option[Yaml]): Option[List[Yaml]] =
    this.array.flatMap { _.traverse(f) }

  def scalar: Option[String] = this match {
    case YScalar(raw) => Some(raw)
    case _ => None
  }

  def assoc: Option[Map[Yaml, Yaml]] = this match {
    case YMap(map) => Some(map)
    case _ => None
  }

  def array: Option[List[Yaml]] = this match {
    case YSeq(seq) => Some(seq)
    case _ => None
  }

  def stream: Option[Stream[Yaml]] = this match {
    case YStream(stream) => Some(stream)
    case _ => None
  }

  def nul: Option[Unit] = this match {
    case YScalar(s) if matchNull(s) => Some(())
    case _ => None
  }

  def nan: Option[Unit] = this match {
    case YScalar(s) if matchNan(s) => Some(())
    case _ => None
  }

  def inf: Option[Unit] = this match {
    case YScalar(s) if matchInf(s) => Some(())
    case _ => None
  }

  def neginf: Option[Unit] = this match {
    case YScalar(s) if matchNegInf(s) => Some(())
    case _ => None
  }

  def bool: Option[Boolean] = this match {
    case YScalar(s) if matchTrue(s) => Some(true)
    case YScalar(s) if matchFalse(s) => Some(false)
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
    case YScalar(s) => Some(s)
    case _ => None
  }

  // if all map keys are string, will be well-formed JSON
  def print: String = printYaml(this)

  def pretty: String = prettyPrint(2)

  def prettyPrint(spaces: Int): String = prettyPrintYaml(spaces, this)

  // returns None if the string is not well-formed JSON
  def printJson: Option[String] = printJsonYaml(this)
}

sealed trait ToYaml {
  def toYaml: Yaml
}

object Yaml {

  import YamlS._

  def parse(raw: String): Option[Yaml] = YamlS.parseYaml(raw)

  def nul: Yaml = YScalar("~")
  def nan: Yaml = YScalar(".nan")
  def inf: Yaml = YScalar(".inf")
  def neginf: Yaml = YScalar("-.inf")
  def bool(p: Boolean): Yaml = YScalar(if (p) "true" else "false")
  def long(n: Long): Yaml = YScalar(n.toString)
  def double(x: Double): Yaml = YScalar(x.toString)
  def string(s: String): Yaml = YScalar(s)
  def scalar(s: String): Yaml = YScalar(s)
  def array(seq: List[Yaml]): Yaml = YSeq(seq)
  def assoc(map: Map[Yaml, Yaml]): Yaml = YMap(map)
  def stream(stream: Stream[Yaml]): Yaml = YStream(stream)

  def emptyObj: Yaml = assoc(Map())

  def emptyArr: Yaml = array(List())

  def obj(pairs: (String, ToYaml)*): Yaml =
    assoc(pairs.map(x => (scalar(x._1), x._2.toYaml)).toMap)

  def arr(elements: ToYaml*): Yaml =
    array(elements.map(_.toYaml).toList)
}

object YamlImplicits {

  import Yaml._

  implicit class YamlToYaml(x: Yaml) extends ToYaml {
    def toYaml: Yaml = x
  }

  implicit class UnitToYaml(x: Unit) extends ToYaml {
    def toYaml: Yaml = nul
  }

  implicit class BoolToYaml(x: Boolean) extends ToYaml {
    def toYaml: Yaml = bool(x)
  }

  implicit class StringToYaml(x: String) extends ToYaml {
    def toYaml: Yaml = string(x)
  }

  implicit class IntToYaml(x: Int) extends ToYaml {
    def toYaml: Yaml = long(x.toLong)
  }

  implicit class LongToYaml(x: Long) extends ToYaml {
    def toYaml: Yaml = long(x)
  }

  implicit class FloatToYaml(x: Float) extends ToYaml {
    def toYaml: Yaml = double(x.toDouble)
  }

  implicit class DoubleToYaml(x: Double) extends ToYaml {
    def toYaml: Yaml = double(x)
  }

  implicit class YamlOp(x: Option[Yaml]) {
    def ~>(key: Yaml): Option[Yaml] = x.flatMap(y => y ~> key)
    def ~>(key: String): Option[Yaml] = x.flatMap(y => y ~> key)
    def ~>(key: Int): Option[Yaml] = x.flatMap(y => y ~> key)
    def ><(f: Yaml => Option[Yaml]): Option[List[Yaml]] =
      x.array.flatMap { _.traverse(f) }
    def assoc: Option[Map[Yaml, Yaml]] = x.flatMap(y => y.assoc)
    def array: Option[List[Yaml]] = x.flatMap(y => y.array)
    def stream: Option[Stream[Yaml]] = x.flatMap(y => y.stream)
    def nul: Option[Unit] = x.flatMap(y => y.nul)
    def nan: Option[Unit] = x.flatMap(y => y.nan)
    def inf: Option[Unit] = x.flatMap(y => y.inf)
    def neginf: Option[Unit] = x.flatMap(y => y.neginf)
    def bool: Option[Boolean] = x.flatMap(y => y.bool)
    def long: Option[Long] = x.flatMap(y => y.long)
    def double: Option[Double] = x.flatMap(y => y.double)
    def string: Option[String] = x.flatMap(y => y.string)
  }

  implicit class JsonTraversalList(x: List[Yaml]) {
    def ><(f: Yaml => Option[Yaml]): Option[List[Yaml]] =
      x.traverse(_ >< f).map(_.flatten)
  }

  implicit class JsonTraversalListOp(opX: Option[List[Yaml]]) {
    def ><(f: Yaml => Option[Yaml]): Option[List[Yaml]] =
      opX.flatMap { _.traverse(_ >< f).map(_.flatten) }
  }
}

private object YamlS {

  case class YScalar(get: String) extends Yaml
  case class YSeq(get: List[Yaml]) extends Yaml
  case class YMap(get: Map[Yaml, Yaml]) extends Yaml
  case class YStream(get: Stream[Yaml]) extends Yaml

  def parseYaml(raw: String): Option[Yaml] = {

    import org.yaml.snakeyaml.{Yaml => SnakeYaml}
    import scala.collection.JavaConversions._

    def parseDoc(a: Any): Option[Yaml] = a match {
      case a: java.util.Map[_, _] =>
        parseMap(a.toMap)
      case a: java.util.List[_] =>
        parseList(a.toList)
      case a: java.util.Date =>
        safely(YScalar(new java.text.SimpleDateFormat("yyyy-MM-dd").format(a)))
      case _ =>
        safely(YScalar(a.toString))
    }

    def parseMap(map: Map[_, _]): Option[Yaml] =
      map.toList.traverse { case (a, b) =>
        for { k <- parseDoc(a); v <- parseDoc(b) } yield (k, v)
      } map { dict => YMap(dict.toMap) }

    def parseList(list: List[_]): Option[Yaml] =
      list.traverse(parseDoc).map(YSeq)

    def parseStream(stream: Stream[_]): Option[Yaml] =
      stream.traverse(parseDoc).map(YStream)

    safely(new SnakeYaml().load(raw)) match {
      case Some(a) => parseDoc(a)
      case None =>
        safely(new SnakeYaml().loadAll(raw)).flatMap(it => parseStream(it.toStream))
    }
  }

  def printYaml(yaml: Yaml): String = yaml.fold(
    withScalar = scalar => printScalar(forceEscapes = true, raw = scalar),
    withSeq = "[" + _.map(_.print).mkString(", ") + "]",
    withMap = {
      def f(kv: (Yaml, Yaml)): String = kv._1 match {
        case y@YScalar(_) =>
          y.print + ": " + kv._2.print
        case y => "? " + y.print + ": " + kv._2.print
      }
      "{" + _.map(f).mkString(", ") + "}"
    },
    withStream = _.foldLeft("")((s, y) => s + y.print + "\n...\n")
  )

  def prettyPrintYaml(spaces: Int, y: Yaml): String = {

    val tab: String = (1 to spaces).map(_ => " ").mkString

    def helper(indent: String, rest: Yaml): String =
      rest.fold(
        withScalar = scalar =>
          printScalar(forceEscapes = false, raw = scalar),
        withSeq = seq => seq.map({
          case v@YScalar(_) => indent + "- " + v.pretty + "\n"
          case v => "-\n" + helper(indent + tab, v)
        }).mkString,
        withMap = assoc => assoc.map(pair => printPair(indent, pair)).mkString,
        withStream = stream =>
          stream.foldLeft("")((s, y) => s + helper(indent, y) + "...\n")
      )

    def printPair(indent: String, pair: (Yaml, Yaml)): String = pair match {
      case (k, v) => (k.scalar, v.scalar) match {
        case (None, None) => // neither are scalars
          indent + "?\n" + helper(indent + tab, k) + ":\n" + helper(indent + tab, v)
        case (None, Some(_)) => // v is a scalar, k is not
          indent + "?\n" + helper(indent + tab, k) + ": " + v.pretty + "\n"
        case (Some(_), None) => // k is a scalar, v is not
          indent + k.pretty + ":\n" + helper(indent + tab, v)
        case (Some(_), Some(_)) => // both are scalars
          indent + k.pretty + ": " + v.pretty + "\n"
      }
    }

    helper("", y)
  }

  def printScalar(forceEscapes: Boolean, raw: String): String = {

    def escapeIf(raw: String): String = {
      val s = escape(raw)
      if (s.contains("\\")) s else raw
    }

    def escape(raw: String): String = {
      import scala.reflect.runtime.universe._
      Literal(Constant(raw)).toString
    }

    def isFloating: String => Boolean = raw =>
      safely(raw.toDouble.toString.toDouble == raw.toDouble)
        .fold(false)(identity)

    def isIntegral: String => Boolean = raw =>
      safely(raw.toLong.toString.toLong == raw.toLong)
        .fold(false)(identity)

    raw match {
      case _ if matchNull(raw) => "null"
      case _ if matchTrue(raw) => "true"
      case _ if matchFalse(raw) => "false"
      case _ if isIntegral(raw) => raw
      case _ if isFloating(raw) => raw
      case _ => if (forceEscapes) escape(raw) else escapeIf(raw)
    }
  }

  def printJsonYaml(yaml: Yaml): Option[String] = yaml.fold(
    withScalar = _ => safely(yaml.print),
    withSeq = _ => safely(yaml.print),
    withMap = {
      def f(kv: (Yaml, Yaml)): Option[String] = kv._1 match {
        case y@YScalar(_) => safely(y.print + ": " + kv._2.print)
        case _ => None
      }
      assoc =>
        assoc.toList.traverse(f).map(
          "{" + _.mkString(", ") + "}"
        )
    },
    withStream = _ => None
  )

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
