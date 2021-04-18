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
final case class Deprecated(version: String, description: String)

@JsonCodec
case class APIPath(
  path: String,
  methods: List[String] = Nil,
  parts: Map[String, PartDescription] = Map.empty[String, PartDescription],
  params: Map[String, Parameter] = Map.empty[String, Parameter],
  deprecated: Option[Deprecated] = None
) {
  def getPartDocumentation(name: String): String = {
    parts.filter { case (name2, part) =>
      name == name2
    }.foreach(p => return p._2.description)
    ""
  }

  def isDeprecated: Boolean = deprecated.isDefined
}

@JsonCodec
case class APIURL(paths: List[APIPath] = Nil) {
  def params: Map[String, Parameter] =
    paths.filterNot(_.isDeprecated).flatMap(_.params.toList).toMap

  def validPaths: List[APIPath] = paths.filterNot(_.isDeprecated)

  def getPartDocumentation(name: String): String = {
    paths
      .filterNot(_.isDeprecated)
      .flatMap(_.parts.toList)
      .toMap
      .filter { case (name2, part) =>
        name == name2
      }
      .foreach(p => return p._2.description)
    ""
  }

}
