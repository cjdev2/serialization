package local.test.serialization.avro;

import org.apache.avro.io.DecoderFactory;
import org.apache.avro.specific.SpecificDatumReader;

public class MinimalAvroDeserializer {

    public static TestRecord deserialize(byte[] bytes) throws java.io.IOException {
        return new SpecificDatumReader<TestRecord>(TestRecord.getClassSchema())
                .read(null, DecoderFactory.get().binaryDecoder(bytes, null));
    }
}
