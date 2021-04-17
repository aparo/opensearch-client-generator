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

import scala.util.matching.Regex
object CommonStrings {
  val defaultExtractor: Regex = """\(\s*default:\s*(.*?)\)""".r

  def isMultiple(description: String): Boolean = description.contains("comma-separated")

  def extractDefault(mtype: String, description: String): Option[Any] =
    defaultExtractor.findFirstIn(description) match {
      case None => None
      case Some(value) =>
        val result = value.split(":").last.trim.stripSuffix(")")
        if (result.contains(" "))
          return None
        mtype match {
          case "boolean" => Some(result == "true")
          case _         => Some(result)
        }
    }

}
