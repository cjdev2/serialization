package local.test.serialization.avro;

import java.util.Arrays;
import java.util.Optional;

import com.cj.serialization.avro.Java;

public class AvroTestJ {

    public static void main(String[] args) {

        AvroTestJ tests = new AvroTestJ();

        tests.serializeAvro_should_serialize_TestRecords();
        tests.AvroSerializerJ_should_serialize_TestRecords();
        tests.deserializeAvro_should_deserialize_TestRecords();
        tests.AvroDeserializerJ_should_deserialize_TestRecords();
    }

    private void _assert_(String msg, Boolean p) {
        if (!p) {
            throw new RuntimeException(msg);
        }
    }

    private void serializeAvro_should_serialize_TestRecords() {
        // given
        TestRecord record = new TestRecord("foó", 123L);
        byte[] expectedBytes = { 8, 102, 111, -61, -77, -10, 1 };

        // when
        byte[] actualBytes = Java.serializeAvro(record);

        // then
        _assert_("serializeAvro_should_serialize_TestRecords",
                Arrays.equals(expectedBytes, actualBytes));
    }

    private void AvroSerializerJ_should_serialize_TestRecords() {
        // given
        TestRecord record = new TestRecord("foó", 123L);
        Java.AvroSerializerJ<TestRecord> serializer = new Java.AvroSerializerJ<>();
        byte[] exptectedBytes = { 8, 102, 111, -61, -77, -10, 1 };

        // when
        byte[] actualBytes = serializer.serialize(record);

        // then
        _assert_("AvroSerializerJ_should_serialize_TestRecords",
                Arrays.equals(exptectedBytes, actualBytes));
    }

    private void deserializeAvro_should_deserialize_TestRecords() {
        // given
        byte[] bytes = { 8, 102, 111, -61, -77, -10, 1 };
        TestRecord expectedRecord = new TestRecord("foó", 123L);

        // when
        Optional<TestRecord> result = Java.deserializeAvro(TestRecord.getClassSchema(), bytes);

        // then
        _assert_("deserializeAvro_should_deserialize_TestRecords",
                result.isPresent() && result.get().equals(expectedRecord));
    }

    private void AvroDeserializerJ_should_deserialize_TestRecords() {
        // given
        byte[] bytes = { 8, 102, 111, -61, -77, -10, 1 };
        Java.AvroDeserializerJ<TestRecord> deserializer =
                new Java.AvroDeserializerJ<>(TestRecord.getClassSchema());
        TestRecord expectedRecord = new TestRecord("foó", 123L);

        // when
        Optional<TestRecord> result = deserializer.deserialize(bytes);

        // then
        _assert_("AvroDeserializerJ_should_deserialize_TestRecords",
                result.isPresent() && result.get().equals(expectedRecord));
    }
}
