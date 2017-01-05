# Serialization

Serialization library exposing a typeclass-like functional interface for Scala 2.11 and 2.12.

Copyright © 2016 CJ Affiliate under the terms of the MIT License. See "LICENSE" for details.

This branch is binary-compatible with Scala versions 2.11.x.

At this time, only supports UTF-8 character encoding.

## For Maven Projects

For core features add:

```xml
<dependency>
    <groupId>com.cj</groupId>
    <artifactId>serialization</artifactId>
    <version>0.4-SNAPSHOT</version>
</dependency>
```

For JSON functionality, also add:

```xml
<dependency>
    <groupId>io.argonaut</groupId>
    <artifactId>argonaut_2.11</artifactId>
    <version>6.2-RC2</version>
</dependency>
```

## Usage

Full documentation available upon build at "target/site/scaladocs/index.html"

### Core Features

Import core features as:

```scala
import com.cj.serialization._
```

This will put `serialize` and `deserialize` into scope and will put values of type `Serializable[String]`, `Deserializable[String]`, `Serializable[V]`, and `Deserializable[V]` (where `V` is all of Scala's `AnyVal` types) into scope. The user may create their own implicit objects extending `Serializable[T]` and `Deserializable[T]` for any type `T`, which will expose `serialize` and `deserialize` functions for `T`, respectively.

For example:

```scala
case class Foo(bar: Int)

// implement `serialize`
implicit object FooSerializer extends Serializable[Foo] {
  def serialize(t: Foo): Array[Byte] =
    t.toString.getBytes
}

// implement `deserialize` (always the hard part)
implicit object FooDeserializer extends Deserializable[Foo] {
  def deserialize(bytes: Array[Byte]): Option[Foo] = {
    val string: String = new String(bytes)
    val regex = "Foo\\((\\d+)\\)".r

    string match {
      case regex(int) => Some(Foo(int.toInt))
      case _ => None
    }
  }
}

val fooVal: Foo = Foo(1234)
serialize(fooVal) // returns a value of type `Array[Byte]`

val fooBytes: Array[Byte] = "Foo(5678)".getBytes
deserialize[Foo](fooBytes) // returns a value of type `Option[Foo]`

assert(
  // `Foo` survives a round trip
  deserialize[Foo](serialize(fooVal)) == Some(fooVal)
)
assert(
  // serialized `Foo` survives a round trip
  deserialize[Foo](fooBytes).map(serialize[Foo]).flatMap(deserialize[Foo])
    == deserialize[Foo](fooBytes)
)
```

See "src/test/scala/SerializationDemo.scala" for details.

### JSON Features

In addition to importing core features, import JSON feastures as:

```scala
import argonaut.{Argonaut, Json}
import com.cj.serialization.json._
```

See "src/test/scala/JsonDemo.scala" for details.

### Avro Integration

In addition to importing core features, import Avro integration as:

```scala
import com.cj.serialization.avro._
```

This will put a value of type `Serialize[SpecificRecord]` into scope, which will expose `serialize` to any class that extends from Avro's `SpecificRecord` interface, with no additional user input required.

To deserialize a class `R` that extend `SpecificRecord`, the user may construct an implicit value of the class `AvroDeserializable[R]`, which extends `Deserializable[R]` by simply providing the Avro `Scheme` of `R`. This will expose `deserialize` to `R`, allowing the user to deserialize byte arrays to values of type `Option[R]`. For example:

```scala
// Avro-generated class
class Rec extends SpecificRecord { ... }

val avroRecord: Rec = new Rec( ... )
// we can serialize `Rec`s out of the box. these records are readable by Avro
// clients that are agnostic of this library
val avroRecordSerializedByUs: Array[Byte] = serialize(avroRecord)

// help Avro help you
implicit object RDeserializer
extends AvroDeserializable[Rec](Rec.getClassSchema)

assert(
  // Avro records survive a round trip
  deserialize[R](avroRecordSerializedByUs) == Some(avroRecord)
)

val avroRecordSerializedBySomeoneElse: Array[Byte] = ???
assert(
  // we can deserialize avro records that were serialized by Avro clients that
  // are agnostic of this library
  deserialize[R](avroRecordSerializedBySomeoneElse)
)
```

See "src/test/scala/AvroDemo.scala" for details.
