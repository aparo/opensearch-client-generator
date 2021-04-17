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

import scala.collection.mutable.ListBuffer

import io.circe.Json
import io.circe.derivation.annotations.JsonCodec

sealed trait AbstractParameter {
  def `type`: String

  def description: String

  def options: List[String]

  def default: Option[Any]

  def required: Boolean

  def subtype: Option[String]

  val notMultipleEnums: Set[String] = Set("Bytes", "WaitForStatus")

  def isMultiple: Boolean = CommonStrings.isMultiple(this.description)

  def getType: String = `type` match {
    case s: String if multiple                            => "Seq[String]"
    case "boolean"                                        => "Boolean"
    case "list"                                           => "Seq[String]"
    case "string" | "text" | "enum" | "time" | "duration" => "String"
    case "int"                                            => "Int"
    case "integer"                                        => "Int"
    case "long"                                           => "Long"
    case "number" =>
      subtype match {
        case Some(value) =>
          value match {
            case "long" => "Long"
            case "int"  => "Integer"
          }
        case None =>
          "Double"
      }

    case "JsonObject" => "JsonObject"
    case ""           => "String"
  }

  def requiredToString: String = `type` match {
    case s: String if multiple => ""
    case "string" | "text"     => ""
    case _                     => ".toString"
  }

  lazy val multiple: Boolean =
    CommonStrings.isMultiple(description) || `type` == "list" || subtype.getOrElse("") == "list" ||
      default.getOrElse(None).isInstanceOf[Seq[_]]

  def toGoodName(value: String): String = value.stripPrefix("_") match {
    case value: String if value.indexOf("_", 1) > 0 =>
      val pos = value.indexOf('_', 1)
      value.take(pos) + toGoodName(value.drop(pos + 1).capitalize)
    case value => value
  }

  def getParameterName(name: String): String = name match {
    case "type"   => "`type`"
    case "wait"   => "`wait`"
    case "Format" => "OutputFormat"
    case value    => toGoodName(value)
  }

  def getCookedDefault: String = getType match {
    case "Boolean" | "Integer" | "Numeric" | "Double" | "Long" => default.get.toString
    case _                                                     => "\"" + default.get.toString + "\""
  }

  /* Check both None and null */
  def defaultIsValid: Boolean = default match {
    case None       => false
    case Some(null) => false
    case Some(_)    => true
  }

  def isEnumMultiple(name: String): Boolean = name.endsWith("s") && !notMultipleEnums.contains(name)

  def toQueryParam(name: String): String = {
    val code = new ListBuffer[String]()
    if (multiple && this.`type` != "enum") {
      if (defaultIsValid) {
        //        code += "Seq[String] = "+default.get
        code += "Seq[String] = Nil"
      } else {
        code += "Seq[String] = Nil"
      }
    } else if (!required && !default.isDefined && options.isEmpty) {
      code += s"Option[${getType}] = None"
    } else if (this.`type` == "list") {
      if (defaultIsValid) {
        //        code += "Seq[String] = "+default.get
        code += "Seq[String] = Nil"
      } else {
        code += "Seq[String] = Nil"
      }
    } else if (this.`type` == "enum") {
      val enum = getParameterName(name.capitalize)
      if (isEnumMultiple(enum)) {
        if (defaultIsValid) {
//          val d = default.get match {
//            case s: String             => s"${enum}.$s"
//            case fields: List[_]       => fields.map(s => s"${enum}.${s}").toList.mkString(", ")
//            case fields: ListBuffer[_] => fields.map(s => s"${enum}.${s}").toList.mkString(", ")
//          }
          //          code += s"""Seq[${enum}.${enum}] = Seq(${d})"""
          code += s"""Seq[${enum}] = Nil"""
        } else {
          code += s"""Seq[${enum}] = Nil"""
        }
      } else {
        if (defaultIsValid) {
          code += s"""${enum} = ${enum}.""" + default.get.toString.replace("\"", "")
        } else {
          code += s"""Option[${enum}] = None"""
        }
      }
    } else {
      if (defaultIsValid) {
        code += getType + "=" + getCookedDefault
      } else {
        if (required)
          code += s"${getType}"
        else
          code += s"Option[${getType}] = None"
      }
    }
    code.mkString
  }

  def toBodyCode(name: String): String = {
    val code = new ListBuffer[String]()
    if (multiple) {
      code += s"""    if(${getParameterName(name)}.nonEmpty){
                 |        queryArgs += ("$name" -> ${getParameterName(name)}.toList.mkString(","))
                 |    }
                 |""".stripMargin('|')
    } else if (this.`type` == "enum") {
      val enum      = getParameterName(name)
      val enumClass = getParameterName(name.capitalize)
      val enumCap   = toGoodName(name).capitalize
      if (isEnumMultiple(enumCap)) {
        code += s"""    if($enum.nonEmpty) {"""
        if (defaultIsValid) {
          val d = default.get match {
            case j: Json if j.isString => s"${enum}.${j.asString.get}"
            case j: Json if j.isArray =>
              j.as[List[String]].toOption.getOrElse(Nil).map(s => s"${enum}.${s}").toList.mkString(", ")
          }
          code += s"""
                     |        if($enum.toSet !=  Set(${d.capitalize})){
                     |            queryArgs += ("$name" ->$enum.mkString(","))
                     |        }
          """.stripMargin

        } else {
          code += s"""
                     |        queryArgs += ("$name" -> $enum match {
                     |           case Some(e) => e.mkString(",")
                     |           case e => e.mkString(",")
                     |        })
          """.stripMargin

        }
        code += s"""
                   |    }
          """.stripMargin
      } else if (defaultIsValid) {
        code += s"""    if(${enum}!=${enumClass}.${default.get})
                   |        queryArgs += ("$name" -> ${enum}.toString)
                   |""".stripMargin('|')
      } else {
        code += s"""    $enum.foreach{ v =>
                   |        queryArgs += ("$name" -> v$requiredToString)
                   |    }
                   |""".stripMargin('|')
      }
    } else if (defaultIsValid) {
      code += s"""    if(${getParameterName(name)}!=$getCookedDefault) queryArgs += ("$name" -> ${getParameterName(
        name
      )}$requiredToString)\n"""
    } else {
      code += s"""    ${getParameterName(name)}.foreach{ v=> queryArgs += ("$name" -> v$requiredToString )}\n"""
    }
    code.mkString
  }

  def getValidEnum(name: String): String = name match {
    case "type" => "`type`"
    case "wait" => "`wait`"
    case value  => value
  }

  def getEnum(name: String): Map[String, String] =
    `type` match {
      case "enum" =>
        val newName = getParameterName(name.capitalize)
        Map(newName -> s""" sealed trait $newName extends EnumEntry
                          | case object $newName extends CirceEnum[$newName] with Enum[$newName] {
                          |    ${options
          .map(getValidEnum)
          .map(st => s"  case object $st extends $newName")
          .mkString("\n")}
                          |
                          |    val values = findValues
                          |  }""".stripMargin('|'))
      case _ => Map.empty[String, String]
    }
}

@JsonCodec
case class CallParameter(
  name: String,
  `type`: String,
  description: String,
  options: List[String] = Nil,
  default: Option[Json] = None,
  required: Boolean = false,
  scope: String = "query",
  subtype: Option[String] = None
) extends AbstractParameter {
  def getCookedDocumentation: String = s" * @param $parameterName $description"

  def getDefParameter: String = s"var $parameterName : $toQueryParam"

  def getDefParameterNoVar: String = s"$parameterName: $toQueryParam"

  def parameterName: String = this.getParameterName(this.name)

  def toQueryParam: String = toQueryParam(this.name)

  def toObjectParam: String = toQueryParam(this.name).split("=")(0).trim()

  def toBodyCode: String = toBodyCode(this.name)

}
@JsonCodec
case class Parameter(
  `type`: String,
  description: String,
  options: List[String] = Nil,
  default: Option[Json] = None,
  required: Boolean = false,
  subtype: Option[String] = None
) extends AbstractParameter {

  def toCallParameter(name: String): CallParameter = new CallParameter(
    name,
    `type` = this.`type`,
    description = this.description,
    options = options,
    default = default,
    required = required,
    subtype = subtype
  )

}
