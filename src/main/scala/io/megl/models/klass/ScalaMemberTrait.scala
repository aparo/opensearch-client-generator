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

import com.google.common.base.CaseFormat
import io.megl.models.APIUtils

trait ScalaMemberTrait extends BaseMemberTrait {

  def nameScala: String = CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.LOWER_CAMEL, name)

  def typeScala: String =
    APIUtils.convertToScala(
      `type`,
      required = required,
      multiple = multiple,
      default = default,
      codeName = codeName,
      subType = subType
    )

  def scalaDefault: Option[String] =
    APIUtils.convertToScalaDefault(
      `type`,
      required = required,
      multiple = multiple,
      options = options,
      default = default,
      subType = subType
    )

  def scalaForcedDefault: String = {
    if (default.isDefined) {
      return default.get.noSpaces
    }

    if (required)
      return `type` match {
        case "integer"  => "0"
        case "string"   => "\"\""
        case "jvalue"   => "Json.Null"
        case "jobject"  => "JsonObject(Map.empty[String, Json])"
        case "geopoint" => "new GeoPoint(0.0,0.0)"
      }

    if (multiple) {
      return "Nil"
    }
    "None"
  }

  def baseScalaType: String = APIUtils.baseType(`type`)

  def nameAttributeScala: String = name match {
    case "type"  => "`type`"
    case "match" => "`match`"
    case s: String if s.startsWith("_") =>
      "_" + CaseFormat.LOWER_UNDERSCORE.to(CaseFormat.LOWER_CAMEL, name.substring(1))
    case _ => nameScala
  }

  def toJsonScala: String =
    if (this.isBaseType)
      s"""${this.nameAttributeScala}.asJson"""
    else {
      this.multiple match {
        case true =>
          s"""${this.nameAttributeScala}.map(_.asJson).asJson"""
        case false =>
          s"""${this.nameAttributeScala}.asJson"""
      }
    }

  def readJsonScala: String =
    if (isBaseType) {
      if (multiple) {
        if (required) {
          s""".as[List[${baseScalaType}]]"""
        } else {
          s""".asOpt[List[${baseScalaType}]].getOrElse(Nil)"""
        }
      } else {
        if (required) {
          s""".as[${baseScalaType}]"""
        } else {
          var extra = ""
          if (default.isDefined) {
            extra = s".getOrElse(${scalaDefault.get})"
          }
          s""".asOpt[${baseScalaType}]$extra"""
        }
      }
    } else {
      val baseType      = baseScalaType
      val typeConverter = APIUtils.getScalaTypeConverter(baseType)
      if (multiple) {
        if (required) {
          s""".as[List[Json]].map(v=> ${typeConverter}.fromJson(v))"""
        } else {
          s""".asOpt[List[Json]].getOrElse(Nil).map(v=> ${typeConverter}.fromJson(v))"""
        }
      } else {
        if (required) {
          if (baseType != typeConverter) {
            s""".asOpt[Json].map(${typeConverter}.fromJson).get"""
          } else {
            s""".as[${typeConverter}]"""
          }
        } else {
          if (baseType != typeConverter) {
            s""".asOpt[Json].map(v=> ${typeConverter}.fromJson(v))"""
          } else {
            s""".asOpt[${typeConverter}]"""
          }
        }
      }

    }

}
