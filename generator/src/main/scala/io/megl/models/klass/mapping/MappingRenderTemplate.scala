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

package io.megl.models.klass.mapping

import scala.collection.mutable

import io.megl.Constants
import io.megl.models.APIUtils
import io.megl.models.klass.{ BaseRenderTemplate, ClassDef }

class MappingRenderTemplate(val pathScope: String) extends BaseRenderTemplate {
  def generateScala(classes: List[ClassDef]): Unit = {
    val destQueriesDir = Constants.devScalaAPIDestPath / pathScope
    if (!destQueriesDir.exists()) destQueriesDir.createDirectories()

    val qClasses = classes.map(c => MappingClassDef(c))
    val body     = new mutable.ListBuffer[String]()

    var includes = Set(
      "import io.circe._",
      "import io.circe.generic.extras._"
    )

    val queryTemplate = elasticsearch.mapping.txt.Mapping
    qClasses.foreach { obj =>
      val rd = queryTemplate.render(obj).toString()
      println(rd)
      body += rd
      includes ++= obj.scalaExtraImports

    }
    APIUtils.saveFile(destQueriesDir / "mappings.scala", (includes.toList ++ body).mkString("\n"))
  }

}
