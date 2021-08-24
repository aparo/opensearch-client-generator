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

import scala.collection.immutable.ListMap

import io.megl.generators.openapi._
import io.megl.models._

final case class OpenAPIGeneratorFromSchema(root: Root, preferPost: Boolean = true) {
  def generator(): Unit = {
    val paths = root.endpoints.take(1).flatMap { endpoint =>
      getPaths(endpoint)
    }

    val types = root.types
      .take(0)
      .map { eType =>
        val props = getProperties(eType)
        eType.name.fullName -> getSchema(eType, props)
      }
      .sortBy(_._1)

    val openapiRef = OpenAPI(
      info = openapi.Info("OpenSearch API Definition", "7.10.3"),
      paths = ListMap(paths: _*),
      components = Some(
        Components(
          schemas = ListMap(types: _*),
          securitySchemes = ListMap(
            SecurityScheme.BASIC -> Right(SecurityScheme.basic)
            //        ,
            //        SecurityScheme.BEARER -> Right(SecurityScheme.bearer),
            //        SecurityScheme.APIKEY -> Right(SecurityScheme.apiKey)
          )
        )
      )
    )
    println(openapiRef.toYaml)
  }

  def getPaths(endPoint: EndPoint): List[(String, PathItem)] =
    endPoint.urls.map { endpointUrl =>
      endpointUrl.path -> buildPathInfo(endpointUrl, endPoint)
    }

  def buildPathInfo(endpointUrl: EndpointUrl, endPoint: EndPoint): PathItem = {
    val description = List(
      endPoint.description,
      endPoint.docUrl,
      endPoint.since.map(v => s"Since: $v").getOrElse(""),
      endPoint.stability.map(v => s"Stability: $v").getOrElse("")
    ).filter(_.nonEmpty).mkString("\n")

    var pi = PathItem(description = Some(description))
    val tags = (endPoint.request.map(_.namespace.split('.').head).toList ++ endPoint.response
      .map(_.namespace.split('.').head)
      .toList).distinct.sorted

    var operation = Operation(
      operationId = endPoint.name,
      summary = Some(endPoint.description),
      responses = buildResponse(endPoint),
      tags = tags,
      requestBody = buildRequest(endPoint)
    )
    val methods: List[String] = if (preferPost) {
      applyPreferPost(endpointUrl.methods)
    } else endpointUrl.methods

    methods.foreach {
      case "POST" => pi = pi.copy(post = Some(operation))
      case "GET"  => pi = pi.copy(get = Some(operation))
      case "PUT"  => pi = pi.copy(put = Some(operation))
      case "HEAD" => pi = pi.copy(head = Some(operation))
    }
    pi
  }

  def applyPreferPost(methods: List[String]): List[String] =
    if (methods.contains("POST") && methods.contains("PUT"))
      methods.filterNot(_ == "PUT")
    else methods

  def buildRequest(endPoint: EndPoint): Option[ReferenceOr[RequestBody]] =
    for {
      responseType <- endPoint.request
      eType        <- root.getType(responseType)
    } yield Right(
      RequestBody(
        description = Some(eType.kind),
        required = Some(endPoint.requestBodyRequired),
        content = ListMap(
          endPoint.accept.map { accept =>
            accept -> MediaType(
              schema = Some(getReference(eType))
            )
          }: _*
        )
      )
    )

  def buildResponse(endPoint: EndPoint): ListMap[ResponsesKey, ReferenceOr[Response]] = {
    val res = for {
      responseType <- endPoint.response
      eType        <- root.getType(responseType)
    } yield Response(
      description = eType.kind,
      content = ListMap(
        endPoint.accept.map { accept =>
          accept -> MediaType(
            schema = Some(getReference(eType))
          )
        }: _*
      )
    )
    ListMap(res.map(v => ResponsesCodeKey(200) -> Right(v)).toList: _*)
  }

  def getReference(eType: ESType): ReferenceOr[Schema] = Left(Reference(eType.name.fullName))

  def getSchema(eType: ESType, props: List[(Boolean, String, ReferenceOr[Schema])]): ReferenceOr[Schema] = Right(
    Schema(
      title = Some(eType.name.name),
      required = props.filter(_._1).map(_._2),
      properties = props.map(p => p._2 -> p._3).toMap,
      `type` = Some(SchemaType.Object)
    )
  )

  def getProperties(esType: ESType): List[(Boolean, String, ReferenceOr[Schema])] =
    esType.inherits
      .flatMap(i => root.getType(i.`type`))
      .flatMap(i => getProperties(i)) ++
      esType.properties.map { p =>
        println(p)
        (p.required, p.name, convertProperty(p))
      }

  def convertProperty(prop: Property): ReferenceOr[Schema] =
    convertTypeProperty(prop.`type`)

  def convertTypeProperty(typeProc: TypeProperty): ReferenceOr[Schema] =
    typeProc match {
      case InstanceType(typ, _) =>
        typ.name match {
          case "string"  => Right(Schema(`type` = SchemaType.String))
          case "boolean" => Right(Schema(`type` = SchemaType.Boolean))
          case "integer" => Right(Schema(`type` = SchemaType.Integer))
          case "long"    => Right(Schema(`type` = Some(SchemaType.Integer), format = Some(SchemaFormat.Int64)))
          case "float"   => Right(Schema(`type` = Some(SchemaType.Number), format = Some(SchemaFormat.Float)))
          case "double"  => Right(Schema(`type` = Some(SchemaType.Number), format = Some(SchemaFormat.Double)))
          case _         => Left(Reference(typ.namespace))
        }
      case DictionaryType(_, value) =>
        Right(Schema(`type` = Some(SchemaType.Object), additionalProperties = Some(convertTypeProperty(value))))
      case UnionType(items) => Right(Schema(oneOf = Some(items.map(i => convertTypeProperty(i)))))
      case NamedValueType(items) =>
        Right(Schema(`type` = Some(SchemaType.Array), oneOf = Some(items.map(i => convertTypeProperty(i)))))
      case ArrayType(value) => Right(Schema(`type` = Some(SchemaType.Array), items = Some(convertTypeProperty(value))))
      case LiteralType(value) =>
        Right(Schema(`type` = Some(SchemaType.String), format = SchemaFormat.withNameInsensitiveOption(value)))
      case UserDefinedValueType() => Right(Schema(`type` = SchemaType.Object))
    }

}
