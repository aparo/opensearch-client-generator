package io.megl.models

import better.files.File
import io.circe.Json

object APIUtils {

  def isBaseType(`type`: String): Boolean =
    `type` match {
      case "boolean" | "bool"                               => true
      case "list"                                           => true
      case "map"                                            => true //ok for circe
      case "string" | "text" | "enum" | "time" | "duration" => true
      case "int" | "integer"                                => true
      case "long"                                           => true
      case "double"                                         => true
      case "float"                                          => true
      case "number"                                         => true
      case "jvalue"                                         => true
      case "jobject"                                        => true
      case "geopoint"                                       => true
      case _                                                => false
    }

  def baseType(`type`: String, codeName: Option[String] = None, subType: Option[String] = None): String =
    `type` match {
      case "boolean" | "bool"                      => "Boolean"
      case "list"                                  => "Seq[String]"
      case "string" | "text" | "time" | "duration" => "String"
      case "int"                                   => "Int"
      case "integer"                               => "Int"
      case "jvalue"                                => "Json"
      case "jobject"                               => "JsonObject"
      case "long"                                  => "Long"
      case "number"                                => "Double"
      case "float"                                 => "Float"
      case "double"                                => "Double"
      case "geopoint"                              => "GeoPoint"
      case "map"                                   => s"Map[${subType.getOrElse("String, String")}]"
      case "object" =>
        codeName.map(_.capitalize).getOrElse("missingCodeName")
      case "enum" =>
        codeName.map(_.capitalize).getOrElse("missingCodeName")
      case default => default
    }

  /* we define type builders for models */
  def getScalaTypeConverter(`type`: String): String =
    `type` match {
      case "Filter"    => "Filters"
      case "Query"     => "Queries"
      case "SpanQuery" => "Queries"
      case default     => default
    }

  def convertToScala(
    `type`: String,
    required: Boolean = true,
    multiple: Boolean = true,
    default: Option[Json] = None,
    codeName: Option[String] = None,
    subType: Option[String] = None
  ): String = {
    var result = baseType(`type`, codeName, subType)

    if (multiple) {
      result match {
        case s: String if s.startsWith("Seq[") =>
        case s: String if s.startsWith("Map[") =>
        case s: String                         => result = s"List[$result]"
      }
    } else if (!required && !default.isDefined) {
      result = s"Option[$result]"
    }
    result
  }

  def convertToScalaDefault(
    `type`: String,
    required: Boolean = true,
    multiple: Boolean = true,
    options: Option[List[Json]] = None,
    default: Option[Json] = None,
    subType: Option[String] = None
  ): Option[String] = {
    if (default.isDefined) {
      val value = default.get.noSpacesSortKeys
      return Some(value)
    }

    if (`type` == "map")
      return Some(s"Map.empty[${subType.getOrElse("String, String")}]")

    if (required)
      return None

    if (multiple) {
      return Some("Nil")
    }
    Some("None")
  }

  def convertToScalaJSDefault(
    `type`: String,
    required: Boolean = true,
    multiple: Boolean = true,
    options: Option[List[Json]] = None,
    default: Option[Json] = None
  ): Option[String] = {
    if (default.isDefined) {
      val value = default.get.noSpacesSortKeys
      return Some(value)
    }

    if (required)
      return None

    if (multiple) {
      return Some("Nil")
    }
    Some("null")
  }

  def baseTypeJS(`type`: String): String =
    `type` match {
      case "boolean" | "bool"                               => "Boolean"
      case "list"                                           => "Seq[String]"
      case "string" | "text" | "enum" | "time" | "duration" => "String"
      case "int"                                            => "Int"
      case "integer"                                        => "Int"
      case "jvalue"                                         => "js.Any"
      case "jobject"                                        => "js.Dictionary[js.Any]"
      case "long"                                           => "Long"
      case "number"                                         => "Double"
      case "float"                                          => "Float"
      case "double"                                         => "Double"
      case "geopoint"                                       => "GeoPoint"
      case default                                          => default
    }

  def convertToScalaJS(
    `type`: String,
    required: Boolean = true,
    multiple: Boolean = true,
    default: Option[Json] = None
  ): String = {
    var result = baseTypeJS(`type`)

    if (multiple) {
      if (!result.startsWith("Seq")) {
        result = s"List[$result]"
      }
    }
    if (!required && !multiple && !default.isDefined) {
      result = s"Option[$result]"
    }
    result
  }

  def convertToTSDefault(
    `type`: String,
    required: Boolean = true,
    multiple: Boolean = true,
    options: Option[List[Json]] = None,
    default: Option[Json] = None
  ): Option[String] = {
    if (default.isDefined) {
      val value = default.get.noSpacesSortKeys
      return Some(value)
    }

    if (required)
      return None

    if (multiple) {
      return Some("[]")
    }
    Some("null")
  }

  def baseTypeTS(`type`: String): String =
    `type` match {
      case "boolean" | "bool"                               => "boolean"
      case "list"                                           => "string[]"
      case "string" | "text" | "enum" | "time" | "duration" => "string"
      case "int"                                            => "number"
      case "integer"                                        => "number"
      case "jvalue"                                         => "any"
      case "jobject"                                        => "{}"
      case "long"                                           => "number"
      case "number"                                         => "number"
      case "float"                                          => "number"
      case "double"                                         => "number"
      case "geopoint"                                       => "GeoPoint"
      case default                                          => default
    }

  def convertToTS(
    `type`: String,
    required: Boolean = true,
    multiple: Boolean = true,
    default: Option[Json] = None
  ): String = {
    var result = baseTypeTS(`type`)

    if (multiple) {
      if (!result.startsWith("Seq")) {
        result = s"$result[]"
      }
    }
    if (!required && !multiple && !default.isDefined) {
      result = s"$result" //option
    }
    result
  }

  def saveFile(filename: File, content: String): Unit = {

    if (!filename.parent.exists()) {
      filename.parent.createDirectories()
    }

    filename.writeText(content)

    println(s"Generated ${filename}")

  }
}
