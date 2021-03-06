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

package io.megl.generators.openapi

import scala.collection.immutable.ListMap

import io.circe.derivation.annotations.JsonCodec
import io.circe.syntax._
import io.circe.yaml.Printer
import io.megl.common.CirceUtils

@JsonCodec
final case class OpenAPI(
  openapi: String = "3.0.3",
  info: Info,
  tags: List[Tag] = Nil,
  servers: List[Server] = Nil,
  paths: ListMap[String, PathItem] = ListMap.empty,
  components: Option[Components] = None,
  security: List[SecurityRequirement] = Nil
) {

  def addPathItem(path: String, pathItem: PathItem): OpenAPI = {
    val pathItem2 = paths.get(path) match {
      case None           => pathItem
      case Some(existing) => existing.mergeWith(pathItem)
    }

    copy(paths = paths + (path -> pathItem2))
  }

  def setServers(s: List[Server]): OpenAPI = copy(servers = s)

  def setTags(t: List[Tag]): OpenAPI = copy(tags = t)

  def toYaml: String =
    Printer(dropNullKeys = true, preserveOrder = true).pretty(CirceUtils.cleanValue(this.asJson))

}
