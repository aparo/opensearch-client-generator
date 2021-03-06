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

import better.files.File
import com.typesafe.scalalogging.LazyLogging
import io.megl.parsers.jsonspec.APIEntry
import org.scalablytyped.converter.internal.InFile
import org.scalablytyped.converter.internal.ts._

import scala.collection.immutable

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

  def parseEntities(): Seq[TSFileMetadata] = {
    Constants.specifications.listRecursively
      .filter(_.isRegularFile())
      //      .filter(_.name.endsWith("common.ts"))
//      .filter(_.name.endsWith("CatAllocationRecord.ts"))
//      .filter(_.name.endsWith("behaviors.ts"))
//      .filter(_.name.equals("Aggregate.ts"))
      .filter(_.name.endsWith(".ts"))
//      .take(10)
      .flatMap { f =>
        logger.debug(s"Loading $f")
        org.scalablytyped.converter.internal.ts.parser.parseFile(InFile(os.Path(f.toJava))) match {
          case Left(value) =>
            println(f)
            println(value)
            None
          case Right(value) =>
            Some(f -> value)
        }
      }
      .toList.flatMap(r => convertToClass(r._1, r._2))
  }

  def convertToClass(file: File, parsed: TsParsedFile): immutable.Seq[TSFileMetadata] = {
    parsed.members.toList.flatMap {
      c =>
        c match {
          case decl: TsDecl =>
            decl match {
              case decl2: TsNamedDecl =>
                decl2 match {
                  case TsDeclNamespace(comments, declared, name, members, codePath, jsLocation) => None
                  case c:TsDeclClass => Some(TSFileMetadata(file, c))
                  case c:TsDeclInterface =>
                    Some(TSFileMetadata(file, c))
                  case c:TsDeclEnum =>
                    Some(TSFileMetadata(file, c))
                  case TsDeclVar(comments, declared, readOnly, name, tpe, expr, jsLocation, codePath) => None
                  case TsDeclFunction(comments, declared, name, signature, jsLocation, codePath) => None
                  case TsDeclTypeAlias(comments, declared, name, tparams, alias, codePath) => None
                  case decl: TsNamedValueDecl => None
                }
              case TsImport(typeOnly, imported, from) => None
              case TsExport(comments, typeOnly, tpe, exported) => None
              case TsExportAsNamespace(ident) => None
            }
          case container: TsContainer =>
            container match {
              case TsParsedFile(comments, directives, members, codePath) => None
              case module: TsDeclNamespaceOrModule => None
              case TsGlobal(comments, declared, members, codePath) => None
            }
        }
    }
  }

}
