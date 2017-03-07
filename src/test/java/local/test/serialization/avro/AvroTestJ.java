package local.test.serialization.avro;

import java.util.Arrays;
import java.util.Optional;

import com.cj.serialization.avro.Java;

public class AvroTestJ {

    public static void main(String[] args) {

        AvroTestJ tests = new AvroTestJ();

        tests.serialize_should_be_compatible_with_TestRecord();
        tests.AvroSerializerJ_should_be_compatible_with_TestRecord();
        tests.deserialize_should_be_compatible_with_TestRecord();
        tests.AvroDeserializerJ_should_be_compatible_with_TestRecord();
    }

    private void _assert_(Boolean p) {
        if (!p) {
            throw new RuntimeException("Test Failure!");
        }
    }

    private void serialize_should_be_compatible_with_TestRecord() {
        // given
        TestRecord record = new TestRecord("foó", 123L);
        byte[] expectedBytes = { 8, 102, 111, -61, -77, -10, 1 };

        // when
        byte[] actualBytes = Java.serialize(record);

        // then
        _assert_(Arrays.equals(expectedBytes, actualBytes));
    }

    private void AvroSerializerJ_should_be_compatible_with_TestRecord() {
        // given
        TestRecord record = new TestRecord("foó", 123L);
        Java.AvroSerializerJ<TestRecord> serializer = new Java.AvroSerializerJ<>();
        byte[] exptectedBytes = { 8, 102, 111, -61, -77, -10, 1 };

        // when
        byte[] actualBytes = serializer.serialize(record);

        // then
        _assert_(Arrays.equals(exptectedBytes, actualBytes));
    }

    private void deserialize_should_be_compatible_with_TestRecord() {
        // given
        byte[] bytes = { 8, 102, 111, -61, -77, -10, 1 };
        TestRecord expectedRecord = new TestRecord("foó", 123L);

        // when
        Optional<TestRecord> result = Java.deserialize(TestRecord.getClassSchema(), bytes);

        // then
        _assert_(result.isPresent() && result.get().equals(expectedRecord));
    }

    private void AvroDeserializerJ_should_be_compatible_with_TestRecord() {
        // given
        byte[] bytes = { 8, 102, 111, -61, -77, -10, 1 };
        Java.AvroDeserializerJ<TestRecord> deserializer =
                new Java.AvroDeserializerJ<>(TestRecord.getClassSchema());
        TestRecord expectedRecord = new TestRecord("foó", 123L);

        // when
        Optional<TestRecord> result = deserializer.deserialize(bytes);

        // then
        _assert_(result.isPresent() && result.get().equals(expectedRecord));
    }
}
