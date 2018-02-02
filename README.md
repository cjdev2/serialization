# Serialization

Serialization library exposing a typeclass-like functional interface for Scala 2.11 and 2.12.

Copyright © 2017 CJ Affiliate under the terms of the MIT License. See "LICENSE" for details.

This branch is binary-compatible with Scala versions 2.11.x.

Currentl limitations: 
- Only supports UTF-8 character encoding.
- YAML support is incomplete and experimental.

## For Maven Projects

For core features add:

```xml
<dependency>
    <groupId>com.cj</groupId>
    <artifactId>serialization_2.11</artifactId>
    <version>1.4.2</version>
</dependency>
```

Avro, Thrift, and Scrooge are not added to you project dependencies by default. To use them, add them to your POM as normal. E.g.:

```xml
<dependency>
    <groupId>org.apache.avro</groupId>
    <artifactId>avro</artifactId>
    <version>1.8.2</version>
</dependency>
```

or

```xml
<dependency>
    <groupId>org.apache.thrift</groupId>
    <artifactId>libthrift</artifactId>
    <version>0.10.0</version>
    <optional>true</optional>
</dependency>
<dependency>
    <groupId>com.twitter</groupId>
    <artifactId>scrooge-core_2.11</artifactId>
    <version>4.20.0</version>
    <optional>true</optional>
</dependency>
```

## Usage

Full documentation available upon build at "target/site/scaladocs/index.html"

### Core Features

The basic notions of the library are the traits `Serialize[T]` and `Deserialize[T]`, and their methods `serialize` and `deserialize`.

```scala
trait Serialize[-T] {
  def serialize(t: T): Array[Byte]
}
trait Deserialize[+T] {
  def deserialize(bytes: Array[Byte]): Option[T]
}

def serialize[T: Serialize](t: T): Array[Byte]
def deserialize[T: Deserialize](bytes: Array[Byte]): Option[T]
```

Import core features as:

```scala
import com.cj.serialization._
```

This will put `serialize` and `deserialize` into scope and will put values of type `Serialize[String]`, `Deserialize[String]`, `Serialize[V]`, and `Deserialize[V]` (where `V` is all of Scala's `AnyVal` types) into scope. The user may create their own values of `Serialize[T]` and `Deserialize[T]` for any type `T`, which will expose `serialize` and `deserialize` prefix functions for `T`, respectively.

For example:

```scala
case class Foo(bar: Int)

// implement `serialize`
implicit object FooSerializer extends Serialize[Foo] {
  def serialize(t: Foo): Array[Byte] = t.toString.getBytes
}

// implement `deserialize`
implicit object FooDeserializer extends Deserialize[Foo] {
  def deserialize(bytes: Array[Byte]): Option[Foo] = {
    val string: String = new String(bytes)
    val regex = "Foo\\((\\d+)\\)".r

    string match {
      case regex(digits) => Option(Foo(digits.toInt))
      case _ => None
    }
  }
}

val fooVal: Foo = Foo(1234)
val fooBytes: Array[Byte] = "Foo(5678)".getBytes
val incoherentBytes: Array[Byte] = "bytes".getBytes

assert(
  // `fooVal' serializes to `"Foo(1234)".getBytes`
  serialize(fooVal) sameElements "Foo(1234)".getBytes
)
assert(
  // 'fooBytes' deserializes to `RSuccess(Foo(5678)`
  deserialize[Foo](fooBytes).contains(Foo(5678))
)
assert(
  // `deserialize` fails gracefully on incoherent input
  deserialize[Foo](incoherentBytes).isEmpty
)
```

See "src/test/scala/SerializationDemo.scala" for details.

### JSON Features

_Serialization_ provides a native Scala AST for representing JSON and embedded DSLs for constructing and destructuring JSON values, using the fabulous _Argonaut_ library as a backend. We do a pretty good job or wrapping _Argonaut_, so no outside knowledge is required to use these features (though it can help with automatic codec generation, see below).

```scala
import com.cj.serialization.json._, JsonImplicits._

val jsonString = """{"foo": "bar", "baz": [1,2,3]}"""
val jsonAST = Json.obj("foo" -> "bar", "baz" -> Json.arr(0,1,2))
val foo = (jsonAST ~> "foo").string
val baz = (jsonAST ~> "baz" ~> 2).long

assert {
  foo.contains("bar") &&
  baz.contains(2) &&
  parseJson[Json](jsonString).contains(jsonAST)
}
```

We can piggy-back off of Argonaut's automatic codec generation to automatically be able to convert between native Scala case classes and their JSON representations.

```scala
case class Person(
                   name: String,
                   age: Int,
                   things: List[String],
                   mother: Option[String]
                 )

// use `casecodec4` here because `Person` has four fields
implicit val personCodec: JsonCodec[Person] = JsonCodec(argonaut.CodecJson.derived[Person])
```

Then `personCodec` will be used to convert back and forth between `Person`, `Json`, `String` and `Array[Byte]`

```scala
val tim = Person("Tim Drake", 19, List("Bo", "Rope"), Some("Janet Drake"))
assert(
  prettyJson[Person](tim) ==
    """{
      |  "name" : "Tim Drake",
      |  "age" : 19,
      |  "things" : [
      |    "Bo"
      |  ],
      |  "mother" : "Janet Drake"
      |}""".stripMargin
)

val batmanString =
  """{"name":"Batman","age":38,"things":["Batarang","Batmobile"]}"""
assert(
  parseJson[Person](batmanString) ==
    Some(Person("Batman", 38, List("Batarang", "Batmobile"), None))
)
```

See "src/test/scala/JsonDemo.scala" and "src/test/scala/JsonDemoMinimal.scala" for details.

### Avro Integration

In addition to importing core features, import Avro integration as:

```scala
import com.cj.serialization.avro._
```

This will put a value of type `Serialize[SpecificRecord]` into scope, which will expose `serialize` to any class that extends Avro's `SpecificRecord` interface, with no additional code required.

To deserialize a class `R` that extend `SpecificRecord`, the user may construct a value of the class `DeserializeSpecificRecord[R]`, which extends `Deserialize[R]` by providing the Avro `Scheme` of `R`. This will expose `deserialize` to `R`, allowing the user to deserialize byte arrays generated by any Avro client. For example:

```scala
class Rec extends SpecificRecord { ... }

// you can serialize for free
val rec: Rec = new Rec( ... )
val bytes: Array[Byte] = serialize(rec)

// deserializing requires a bit of boilerplate
implicit object RecD extends DeserializeSpecificRecord[Rec](Rec.getClassSchema)

val incomingBytes: Array[Byte] = ...
val maybeRec: Option[Rec] = deserialize(incomingBytes)
```

One-off convenience methods that construct instances for you are exposed as `serializeAvro` and `deserializeAvro`

```scala
def serializeAvro(record: SpecificRecord): Array[Byte]
def deserializeAvro[T >: Null <: SpecificRecord](schema: Schema, bytes: Array[Byte]): Option[T]
```

See "src/test/scala/AvroDemo.scala" for details.

### Thrift/Scrooge Integration

In addition to importing core features, import Thrift integration (Scrooge integration, really) as:

```scala
import com.cj.serialization.thrift._
```

This will put a value of type `Serialize[ThriftStruct]` into scope, which will expose `serialize` to any class that extends from Scrooge's `ThriftStruct` trait, with no additional user input required.

To deserialize a class `R` that extends `ThriftStruct`, the user may construct a value of the class `ThriftDeserializer[R]`, which extends `Deserialize[R]` by simply providing the Scrooge `ThriftStructCodec` of `R` as a constructor argument. This will expose `deserialize` to `R`, allowing the user to deserialize byte arrays generated by any Thrift client. For example:

```scala
trait Rec extends ThriftStruct { ... } // generated by Scrooge
object Rec extends ThriftStructCodec { ... } // generated by Scrooge

// you can serialize for free
val rec: Rec = Rec( ... )
val bytes: Array[Byte] = serialize(rec)

// deserializing requires a bit of boilerplate
implicit object RecD extends ThriftDeserializer[Rec](Rec)

val incomingBytes: Array[Byte] = ...
val maybeRec: Option[Rec] = deserialize(incomingBytes)
```

One-off convenience methods that construct instances for you are exposed as `serializeAvro` and `deserializeAvro`

```scala
def serializeThriftStruct[T <: ThriftStruct](t: T): Array[Byte]
def deserializeThriftStruct[T <: ThriftStruct](codec: ThriftStructCodec[T], bytes: Array[Byte]): Option[T]
```

See "src/test/scala/ThriftDemo.scala" for details.
