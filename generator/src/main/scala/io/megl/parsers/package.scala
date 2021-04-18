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

package io.megl

import com.typesafe.scalalogging.LazyLogging
import io.megl.parsers.jsonspec.APIEntry

package object parsers extends LazyLogging {

  def parseJson(): List[APIEntry] =
    (Constants.specifications / "_json_spec").listRecursively
      .filter(_.name.endsWith(".json"))
      .flatMap { f =>
        logger.debug(s"Loading $f")
        APIEntry.processFile(f) match {
          case Left(value) =>
            println(f)
            println(value)
            None
          case Right(value) =>
            Some(value)
        }
      }
      .toList
      .flatten
      .sortBy(_.name)

}
