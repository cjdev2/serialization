import com.cj.serialization.avro.Java
import local.test.serialization.avro.TestRecord
import org.scalatest.{FlatSpec, Matchers}

class JavaTest extends FlatSpec with Matchers {
  behavior of "Java"

  it should "serialize and deserialize properly" in {
    val record = new TestRecord("string", 1l)

    val bytes = Java.serialize(record)
    val deserializedRecord = Java.deserialize(bytes, TestRecord.getClassSchema)

    deserializedRecord should be(Some(record))
  }
}
