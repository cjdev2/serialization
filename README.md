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
    <artifactId>serialization</artifactId>
    <version>1.2</version>
</dependency>
```

For JSON, also add:

```xml
<dependency>
    <groupId>io.argonaut</groupId>
    <artifactId>argonaut_2.11</artifactId>
    <version>6.2</version>
</dependency>
```

## Usage

Full documentation available upon build at "target/site/scaladocs/index.html"

### Core Features

The basic notions of the library are the traits `Serialize[T]` and `Deserialize[T]`, the functions `serialize` and `deserialize`, and the wrapper `Result[T]`.

```scala
trait Serialize[-T] {
  def serialize(t: T): Array[Byte]
}
trait Deserialize[+T] {
  def deserialize(bytes: Array[Byte]): Result[T]
}

def serialize[T: Serialize](t: T): Array[Byte]
def deserialize[T: Deserialize](bytes: Array[Byte]): Result[T]

class Result[T] {
  def fold[X](withFailure: String => X, withSuccess: T => X): X
  ...  
}
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
  def serialize(t: Foo): Array[Byte] =
    t.toString.getBytes
}

// implement `deserialize` (always the hard part)
implicit object FooDeserializer extends Deserialize[Foo] {
  def deserialize(bytes: Array[Byte]): Result[Foo] = {
    val string: String = new String(bytes)
    val regex = "Foo\\((\\d+)\\)".r

    string match {
      case regex(int) => Option(Foo(int.toInt))
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
  deserialize[Foo](incoherentBytes).isFailure
)
```

See "src/test/scala/SerializationDemo.scala" for details.

### JSON/Argonaut Integration

The library integrates with and can act as a simple wrapper over Argonaut, a pure functional JSON library for Scala.

Import JSON/Argonaut integration as:

```scala
import argonaut.Argonaut
import com.cj.serialization.json._
```

As the library acts as a simple wrapper, no knowledge of Argonaut is requires to use the JSON features. The one thing you need to learn to do is invoke the `casecodecN` functions, as is done below.

```scala
case class Person(
                   name: String,
                   age: Int,
                   things: List[String],
                   mother: Option[String]
                 )

// use `casecodec4` here because `Person` has four fields
implicit val personCodec: JsonCodec[Person] = JsonCodec(
  Argonaut.casecodec4(Person.apply, Person.unapply)(
  "name", "age", "things", "mother"
  )
)
```

Then `personCodec` can be used to convert back and forth between `Person`, `Json`, `String` and `Array[Byte]`

```scala
val tim = Person("Tim Drake", 19, List("Bo"), Some("Janet Drake"))
assert(
  personCodec.toPrettyJsonString(tim) ==
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
  personCodec.fromJsonString(batmanString) ==
    Some(Person("Batman", 38, List("Batarang", "Batmobile"), None))
)
```

See "src/test/scala/JsonDemo.scala" and "src/test/scala/JsonDemoMinimal.scala" for details.

### Avro Integration

In addition to importing core features, import Avro integration as:

```scala
import com.cj.serialization.avro._
```

This will put a value of type `Serialize[SpecificRecord]` into scope, which will expose `serialize` to any class that extends Avro's `SpecificRecord` interface, with no additional user input required.

To deserialize a class `R` that extend `SpecificRecord`, the user may construct a value of the class `DeserializeAvro[R]`, which extends `Deserialize[R]` by providing the Avro `Scheme` of `R`. This will expose `deserialize` to `R`, allowing the user to deserialize byte arrays generated by any Avro client. For example:

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

See "src/test/scala/ThriftDemo.scala" for details.
