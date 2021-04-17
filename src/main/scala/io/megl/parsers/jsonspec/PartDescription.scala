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

package io.megl.parsers.jsonspec

import io.circe.derivation.annotations.JsonCodec
@JsonCodec
case class PartDescription(`type`: String, description: String, required: Boolean = false)

//object PartDescription {
//
//  implicit val apiPartDescriptionFormat: Format[PartDescription] = Json.format[PartDescription]
//
//  implicit val apiPartDescriptionReader = new Reads[PartDescription] {
//    def reads(js: JsValue): JsResult[PartDescription] = {
//      val t = (js \ "type").as[String]
//      val description = (js \ "description").as[String]
//      val required = (js \ "methods").asOpt[Boolean].getOrElse(true)
//      JsSuccess(PartDescription(t, description, required))
//
//    }
//  }
//
//  def apply2(data: Map[String, Any]): PartDescription = {
//    var part = PartDescription(
//      `type` = data.getOrElse("type", "").asInstanceOf[String],
//      description = data.getOrElse("description", "").asInstanceOf[String])
//    if (data.contains("required")) {
//      part = part.copy(required = data("required") match {
//        case b: Boolean => b
//        case s: String  => println(s); s == "true"
//      })
//    }
//    part
//  }
//}

@JsonCodec
case class APIResult(scala: String)

//object APIResult {
//  implicit val apiResultFormat: Format[APIResult] = Json.format[APIResult]
//}
