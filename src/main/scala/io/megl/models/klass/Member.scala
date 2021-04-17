/*
 * Copyright 2021 Alberto Paro
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.megl.models.klass

import io.circe.Json
import io.circe.derivation.annotations.JsonCodec

@JsonCodec
case class Member(
  name: String,
  var `type`: String,
  var required: Boolean = false,
  var multiple: Boolean = false,
  skip_default: Option[Boolean] = Some(false),
  codeName: Option[String] = None,
  subType: Option[String] = None,
  description: String = "",
  options: Option[List[Json]] = None,
  inField: Option[Boolean] = None,
  var default: Option[Json] = None
) extends ScalaMemberTrait {

  if (name.startsWith("_")) required = false
  if (name == "boost") {
    `type` = "double"
    default = Json.fromDouble(1.0)
  }
}
