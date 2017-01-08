package local.test.serialization.thrift;

import local.test.serialization.thrift.java.TestRecord;
import org.apache.thrift.TDeserializer;

public class MinimalThriftDeserializer {

    public static TestRecord deserialize(byte[] bytes) throws org.apache.thrift.TException {
        TestRecord record = new TestRecord();
        new TDeserializer().deserialize(record, bytes);
        return record;
    }
}
