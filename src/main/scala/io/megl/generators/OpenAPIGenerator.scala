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

import io.megl.generators.openapi
import io.megl.generators.openapi._
import io.megl.models._

import scala.collection.immutable.ListMap

final case class OpenAPIGenerator(root: Root) {
  def generator(): Unit = {
    val types = root.types.map {
      eType =>
        val props = getProperties(eType)
        eType.name.fullName -> getSchema(eType, props)
    }.sortBy(_._1)

    val openapiRef=OpenAPI(info = openapi.Info("OpenSearch API Definition", "7.10.3"),
      components = Some(Components(schemas = ListMap(types:_*), securitySchemes = ListMap(
        SecurityScheme.BEARER -> Right(SecurityScheme.bearer),
        SecurityScheme.APIKEY -> Right(SecurityScheme.apiKey)
      ))))
    println(openapiRef.toYaml)
  }

  def getSchema(eType: ESType, props: List[(Boolean, String, ReferenceOr[Schema])]): ReferenceOr[Schema] = Right(
    Schema(
      title = Some(eType.name.name),
      required = props.filter(_._1).map(_._2),
      properties = props.map(p => p._2 -> p._3).toMap,
      `type` = Some(SchemaType.Object))
  )

  def getProperties(esType: ESType):List[(Boolean, String, ReferenceOr[Schema])] = {
    esType.inherits
      .flatMap(i => root.getType(i.`type`))
      .flatMap(i=> getProperties(i)) ++
    esType.properties.map {
      p =>
        println(p)
        (p.required, p.name, convertProperty(p))
    }
  }


  def convertProperty(prop: Property): ReferenceOr[Schema] = {
    convertTypeProperty(prop.`type`)
  }

  def convertTypeProperty(typeProc:TypeProperty): ReferenceOr[Schema]={
    typeProc match {
      case InstanceType(typ, generics) => typ.name match {
        case "string" => Right(Schema(`type` = SchemaType.String))
        case "boolean" => Right(Schema(`type` = SchemaType.Boolean))
        case "integer" => Right(Schema(`type` = SchemaType.Integer))
        case "long" =>Right(Schema(`type` =Some( SchemaType.Integer), format = Some(SchemaFormat.Int64)))
        case "float" => Right(Schema(`type` =Some( SchemaType.Number), format = Some(SchemaFormat.Float)))
        case "double" => Right(Schema(`type` =Some( SchemaType.Number), format = Some(SchemaFormat.Double)))
          case _ => Left(Reference(typ.namespace))
      }
      case DictionaryType(key, value) =>
        Right(Schema(`type` = Some(SchemaType.Object), additionalProperties = Some(convertTypeProperty(value))))
      case UnionType(items) => Right(Schema(oneOf = Some(items.map(i => convertTypeProperty(i)))))
      case NamedValueType(items) =>  Right(Schema(`type` = Some(SchemaType.Array), oneOf = Some(items.map(i => convertTypeProperty(i)))))
      case ArrayType(value) => Right(Schema(`type` = Some(SchemaType.Array), items = Some(convertTypeProperty(value))))
      case LiteralType(value) => Right(Schema(`type` = Some(SchemaType.String), format = SchemaFormat.withNameInsensitiveOption(value)))
      case UserDefinedValueType() => Right(Schema(`type` = SchemaType.Object))
    }
  }

}
