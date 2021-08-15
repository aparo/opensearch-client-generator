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

package io.megl.generators

import better.files.File
import io.megl.parsers.jsonspec.APIEntry
import org.scalablytyped.converter.internal.ts.TsNamedDecl

case class GeneratorContext(
                           apiEntries:List[APIEntry],
                           entities:Seq[(File, TsNamedDecl)]
                           )

object GeneratorContext{
  def init():GeneratorContext={
    val apis: List[APIEntry] = io.megl.parsers.parseJson()
    GeneratorContext(apiEntries = apis, entities = Nil)
  }
}
