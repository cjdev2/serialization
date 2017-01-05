package local.test.serialization.avro;

import com.sun.xml.internal.bind.marshaller.DataWriter;
import org.apache.avro.file.DataFileWriter;
import org.apache.avro.io.DatumWriter;
import org.apache.avro.io.Encoder;
import org.apache.avro.io.EncoderFactory;
import org.apache.avro.specific.SpecificDatumWriter;
import local.test.serialization.avro.TestRecord;
import org.apache.avro.util.Utf8;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;

public class MinimalAvroSerializerOfTestRecord {


    public static byte[] serialize(TestRecord testRecord) throws IOException {

        DatumWriter<TestRecord> writer = new SpecificDatumWriter<TestRecord>(TestRecord.class);
        OutputStream output = new ByteArrayOutputStream();
        Encoder encoder = EncoderFactory.get().binaryEncoder(output, null);

        writer.write(testRecord, encoder);
        encoder.flush();
        output.close();
        return null;
    }
}
