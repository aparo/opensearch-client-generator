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

import scala.collection.mutable

import better.files._
import io.megl.Constants

trait BaseCodeGenerator {

  val root: File = Constants.devRestAPIPath / "api"

  val managersImports       = new mutable.HashMap[String, List[String]]()
  val managers              = new mutable.HashMap[String, String]()
  val extras                = new mutable.HashMap[String, String]
  val implicits             = new mutable.HashMap[String, List[String]]()
  var files: Iterator[File] = root.listRecursively.filter(_.name.endsWith(".json"))
  lazy val mappingFiles: List[File] =
    (Constants.devRestAPIPath / "mappings").listRecursively.filter(_.name.endsWith(".json")).toList

  def run(): Unit

  protected def processFile(name: File): Either[io.circe.Error, Seq[APIEntry]] =
    if (name.name.startsWith("_"))
      Right(Nil)
    else {
      for {
        json <- io.circe.parser.parse(name.contentAsString)
        obj  <- json.as[Map[String, APIEntry]]
      } yield {
        obj.map(v => v._2.copy(name = v._1)).toSeq
      }
    }

}
