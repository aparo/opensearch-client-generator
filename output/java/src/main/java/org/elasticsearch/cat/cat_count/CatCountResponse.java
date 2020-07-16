
package org.elasticsearch.cat.cat_count;

import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.HashMap;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import org.elasticsearch.Either;
import org.elasticsearch.XContentable;
import org.elasticsearch.NamedContainer;
import org.elasticsearch.common.ParseField;
import org.elasticsearch.common.xcontent.*;
import org.elasticsearch.cat.cat_count.*;

public class CatCountResponse  implements XContentable<CatCountResponse> {
  
  static final ParseField RECORDS = new ParseField("records");
  private List<CatCountRecord> _records;
  public List<CatCountRecord> getRecords() { return this._records; }
  public CatCountResponse setRecords(List<CatCountRecord> val) { this._records = val; return this; }


  
  @Override
  public XContentBuilder toXContent(XContentBuilder builder, ToXContent.Params params) throws IOException {
    builder.startObject();
    if (_records != null) {
      builder.array(RECORDS.getPreferredName(), _records);
    }
    builder.endObject();
    return builder;
  }

  @Override
  public CatCountResponse fromXContent(XContentParser parser) throws IOException, XContentParseException {
    return CatCountResponse.PARSER.apply(parser, null);
  }

  public static final ObjectParser<CatCountResponse, Void> PARSER =
    new ObjectParser<>(CatCountResponse.class.getName(), false, CatCountResponse::new);

  static {
    PARSER.declareObjectArray(CatCountResponse::setRecords, (p, t) -> CatCountRecord.PARSER.apply(p, t), RECORDS);
  }

}