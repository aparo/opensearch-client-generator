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

import scala.collection.mutable.ArrayBuffer

trait BaseClassDefTrait {
  def classDef: ClassDef

  def name: String               = classDef.name
  def className: Option[String]  = classDef.className
  def documentation: String      = classDef.documentation
  def parent: Option[String]     = classDef.parent
  def members: List[Member]      = classDef.members
  def fieldMode: Option[Boolean] = classDef.fieldMode

  def extraCaseClassScala: String
  def extraObjectScala: String

  lazy val hasFieldValues: Boolean = members.exists(_.isInField)

  val scalaTypeToJson: List[(String, String)] = List(
    ("String", "Json.fromString(value)"),
    ("DateTime", "Json.fromString(value.toString)"),
    ("Boolean", "Json.fromBoolean(value)"),
    ("Short", "JsNumber(value.toLong)"),
    ("Int", "JsNumber(value)"),
    ("BigInt", "JsNumber(value.toLong)"),
    ("Double", "JsNumber(value)"),
    ("Float", "JsNumber(value.toDouble)"),
    ("Long", "JsNumber(value)"),
    ("BigDecimal", "JsNumber(value)")
  )

  def scalaExtraImports: List[String] =
    members.flatMap { m =>
      m.typeScala match {
        case s: String if s.contains("GeoPoint") =>
          Some("import elasticsearch.geo.GeoPoint")
        case s: String if s.contains("DateTime") =>
          Some("import java.time.OffsetDateTime")
        case _ => None
      }
    }.distinct.sorted

  def scalaClassSignature: String =
    members.map { m =>
      val key = if (m.nameAttributeScala != m.name) s"""@Key("${m.name}") """ else ""
      val res = s"$key${m.nameAttributeScala}: ${m.typeScala}"
      m.scalaDefault match {
        case None    => res
        case Some(s) => s"$res = $s"
      }
    }.mkString(",\n" + " " * 12 + " " * className.get.length)

  def innerJsonScala: String = {
    val result                = new ArrayBuffer[String]()
    var emitted: List[Member] = Nil

    var startJson            = "var jfields:List[(String, Json)]="
    var singleFieldSerialize = false // to manage inField field:value

    //we manage values in field
    if (hasFieldValues) {
      emitted ++= members.filter(_.name == "field")
      //required
      var inFieldMembers =
        members.filter(m => m.required || m.isBaseType).filter(_.isInField).filterNot(emitted.contains)
      singleFieldSerialize =
        inFieldMembers.length == 1 && (inFieldMembers.head.name == "value" || inFieldMembers.head.name == "values")
      if (!singleFieldSerialize) {
        if (inFieldMembers.isEmpty) {
          result += s"var inFieldJson=Json.obj()"
        } else {
          result += s"var inFieldJson=JsonUtils.jsClean(${serializeRequiredJsonScala(inFieldMembers)})"

        }

        emitted ++= inFieldMembers

        //not required
        inFieldMembers = members.filterNot(_.required).filter(_.isInField).filterNot(emitted.contains)
        result ++= serializeNonRequiredJsonScala(inFieldMembers, "inFieldJson")
        emitted ++= inFieldMembers

        startJson += """List("""
        result += startJson
      }
    } else {
      startJson += "List("
      result += startJson
    }

    val requiredMembers = members
      .filter(m => m.required /* || m.isBaseType */ )
      .filterNot(_.default.isDefined)
      .filterNot(_.isInField)
      .filterNot(emitted.contains)

    if (hasFieldValues) {
      startJson = ""
      if (requiredMembers.nonEmpty)
        startJson = ","
      if (singleFieldSerialize) {
        val value = members.filter(_.required).filter(_.isInField).filterNot(emitted.contains).head
        result += "var json = JsonUtils.jsClean("
        result += s"    field -> ${value.toJsonScala}" + startJson
        emitted ::= value
      } else {
        result += s"    field -> inFieldJson" + startJson
      }

    }
    if (requiredMembers.nonEmpty) {
      result += "    " + serializeRequiredJsonScala(requiredMembers)
      emitted ++= requiredMembers

    }
    result += ")"

    result ++= serializeNonRequiredJsonScala(members.filterNot(_.required).filterNot(emitted.contains))

    result += "Json.obj(jfields:_*)"
    result.map(s => "        " + s).mkString("\n")
  }

  protected def serializeRequiredJsonScala(members: List[Member]): String =
    members.map { m =>
      s"""    "${m.name}" -> ${m.toJsonScala}"""
    }.mkString(",\n")

  protected def serializeNonRequiredJsonScala(
    members: List[Member],
    jsonContainer: String = "jfields"
  ): List[String] = {
    val result = new ArrayBuffer[String]()

    members.foreach { member =>
      if (member.isBaseType) {
        member.multiple match {
          case true =>
            result += s"if(${member.nameAttributeScala}.nonEmpty){"
            result += s"""   $jsonContainer ::= ("${member.name}" -> ${member.nameAttributeScala}.asJson)"""
            result += "}"
            result += ""

          case false =>
            if (member.default.isDefined) {
//                if(member.skip_default.getOrElse(false)) {
              member.scalaDefault.get match {
                case "false" =>
                  result += s"if(${member.nameAttributeScala}){"
                  result += s"""   $jsonContainer ::= ("${member.name}" -> ${member.nameAttributeScala}.asJson)"""
                case "true" =>
                  result += s"if(!${member.nameAttributeScala}){"
                  result += s"""   $jsonContainer ::= ("${member.name}" -> ${member.nameAttributeScala}.asJson)"""
                case _ =>
                  result += s"if(${member.nameAttributeScala} != ${member.scalaDefault.get}){"
                  result += s"""   $jsonContainer ::= ("${member.name}" -> ${member.nameAttributeScala}.asJson)"""
              }
              result += "}"
//                } else {
//                  result += s"""   $jsonContainer ::= ("${member.name}" -> ${member.nameAttributeScala}.asJson)"""
//                }

            } else {
              result += s"${member.nameAttributeScala}.foreach{ value=>"
              result += s"""   $jsonContainer ::= ("${member.name}" -> value.asJson)"""
              result += "}"
            }
            result += ""

        }

      } else {
        member.multiple match {
          case true =>
            result += s"if(${member.nameAttributeScala}.nonEmpty){"
            result += s"""   $jsonContainer ::= ("${member.name}" -> ${member.nameAttributeScala}.map(_.asJson).asJson)"""
            result += "}"
            result += ""

          case false =>
            result += s"${member.nameAttributeScala}.foreach{ value=>"
            result += s"""   $jsonContainer ::= ("${member.name}" -> value.asJson)"""
            result += "}"
            result += ""
        }

      }

    }
    result.toList
  }

  def renderToJSON: String =
    (" def toJson:Json={\n" :: innerJsonScala :: "\n }" :: Nil).mkString("\n")
}
