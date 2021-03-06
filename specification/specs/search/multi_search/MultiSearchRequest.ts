/*
 * Licensed to Elasticsearch B.V. under one or more contributor
 * license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright
 * ownership. Elasticsearch B.V. licenses this file to you under
 * the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

/**
 * @rest_spec_name msearch
 * @since 0.0.0
 * @class_serializer MultiSearchFormatter
 * @stability TODO
 */
interface MultiSearchRequest extends RequestBase {
  path_parts?: {
    index?: Indices
    type?: Types
  }
  query_parameters?: {
    ccs_minimize_roundtrips?: boolean
    max_concurrent_searches?: long
    max_concurrent_shard_requests?: long
    pre_filter_shard_size?: long
    search_type?: SearchType
    total_hits_as_integer?: boolean
    typed_keys?: boolean
  }
  body?: {
    operations?: Dictionary<string, SearchRequest>
  }
}
