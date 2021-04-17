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

package io.megl.models.klass.query

import scala.collection.mutable.ArrayBuffer

import io.megl.Constants
import io.megl.models.APIUtils
import io.megl.models.klass.{ BaseRenderTemplate, ClassDef }

class QueryRenderTemplate(val pathScope: String) extends BaseRenderTemplate {
  def generateScala(klasses: List[ClassDef]): Unit = {
    val destQueriesDir = Constants.devScalaAPIDestPath / pathScope
    if (!destQueriesDir.exists()) destQueriesDir.createDirectories()

    val queryTemplate = elasticsearch.query.txt.Query
    val classes       = klasses.map(c => QueryClassDef(c))
    classes
      //.filter(_.name=="bool")
      .foreach { obj =>
        APIUtils.saveFile(destQueriesDir / s"${obj.className.get}.scala", queryTemplate.render(obj).toString())
      //        println(queryTemplate.render(obj))
      }
    //generating queries converter
    scalaLookUp("Queries", "Query", classes)

  }

  private def scalaLookUp(name: String, returnName: String, values: List[QueryClassDef]): Unit = {
    val destSearchDir = Constants.devScalaAPIDestPath / "search"
    if (!destSearchDir.exists()) destSearchDir.createDirectories()
    val lines = new ArrayBuffer[String]
    lines +=
      s"""package elasticsearch.$pathScope
         |
         |import io.circe.Json
         |
         |object $name {
         |  def fromJson(json: Json):$returnName =""".stripMargin
    lines += "     json.as[JsonObject].foreach{"
    lines += "      case (name,jsValue) =>"
    lines += "        name match {"
    lines ++= values
      .sortBy(_.name)
      .map(q => s"""          case "${q.name}" => return ${q.className.get}.fromJson(jsValue)""")

    lines += "        }"
    lines += "     }"
    lines += "  }"

    APIUtils.saveFile(destSearchDir / s"$name.scala", lines.mkString("\n"))
  }
}
