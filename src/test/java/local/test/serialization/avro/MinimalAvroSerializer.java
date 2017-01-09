package local.test.serialization.avro;

import org.apache.avro.io.BinaryEncoder;
import org.apache.avro.io.EncoderFactory;
import org.apache.avro.specific.SpecificDatumWriter;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class MinimalAvroSerializer {

    public static byte[] serialize(TestRecord testRecord) throws IOException {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        BinaryEncoder encoder = EncoderFactory.get().binaryEncoder(output, null);
        new SpecificDatumWriter<TestRecord>(TestRecord.getClassSchema())
                .write(testRecord, encoder);
        encoder.flush();
        output.close();
        return output.toByteArray();
    }
}
