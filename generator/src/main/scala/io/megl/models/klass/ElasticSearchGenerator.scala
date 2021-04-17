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

import scala.collection.mutable

import io.megl.Constants

class ElasticSearchGenerator(val pathScope: String, val renderTemplate: BaseRenderTemplate) extends BaseGeneratorTrait {

  val root    = Constants.devRestAPIPath
  val destDir = Constants.devScalaAPIDestPath
  destDir.createDirectories()

  lazy val classes: List[ClassDef] = {
    var results = new mutable.ListBuffer[ClassDef]()
    //loading queries
    (root / pathScope).listRecursively.filter(_.name.endsWith(".json")).foreach { filename =>
      logger.info(s"Loading: $filename")
      ClassDef.load(filename) match {
        case Left(value) =>
          println(filename)
          println(value)
        case Right(value) =>
          results += value
      }

    }
    if (results.exists(_.name.startsWith("_"))) {
      val common = results.find(_.name == "_common").get
      results -= common
      results.map(q => q.copy(members = q.members ++ common.members)).sortBy(_.name).toList
    } else
      results.sortBy(_.name).toList

  }

}
