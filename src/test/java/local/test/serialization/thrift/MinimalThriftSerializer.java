package local.test.serialization.thrift;

import local.test.serialization.thrift.java.TestRecord;
import org.apache.thrift.TSerializer;

public class MinimalThriftSerializer {

    public static byte[] serialize(TestRecord t) throws org.apache.thrift.TException {
        return new TSerializer()
                .serialize(t);
    }
}
