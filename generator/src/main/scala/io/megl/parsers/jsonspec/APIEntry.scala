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
import scala.collection.mutable.ListBuffer

import better.files.File
import io.circe.derivation.annotations.{ JsonCodec, JsonKey }
import io.megl.Constants

@JsonCodec
case class APIDocumetation(url: String, description: String)

@JsonCodec
case class APIHeaders(accept: List[String] = Nil, description: List[String] = Nil)

@JsonCodec
case class APIEntry(
  name: String = "undefined",
  documentation: APIDocumetation,
  url: APIURL,
  headers: APIHeaders = APIHeaders(),
  body: Option[APIBody] = None,
  result: Option[APIResult] = None,
  params: Map[String, Parameter] = Map.empty[String, Parameter],
  @JsonKey("native_action") nativeAction: Option[String] = None,
  @JsonKey("native_request") nativeRequest: Option[String] = None,
  @JsonKey("native_response") nativeResponse: Option[String] = None,
  response: Option[APIResponse] = None,
  stability: Option[String] = None,
  visibility: Option[String] = None
) {
  private var path: String                    = ""
  var extra: Map[String, String]              = Map.empty[String, String]
  var implicits: List[String]                 = List.empty[String]
  private var parameters: List[CallParameter] = List.empty[CallParameter]

  // from indices.get_field_mapping -> IndicesGetFieldMapping
  val className: String = {
    val n = name.split('.').flatMap(_.split('_')).map(_.capitalize).mkString
    APIEntry.mappingNames.getOrElse(n, n)
  }
  val scalaRequest: String  = className + "Request"
  val scalaResponse: String = className + "Response"

  parse()

  private def parse(): Unit = {
    val extra      = new mutable.HashMap[String, String]()
    val implicits  = new ListBuffer[String]
    val parameters = new ListBuffer[CallParameter]
    this.url.params.foreach { p =>
      extra ++= p._2.getEnum(p._1)
    }

    //add required in url paths
    val parts = """\{(.*?)\}""".r

    var path: String  = url.validPaths.map(_.path).head
    var requiredPaths = parts.findAllMatchIn(path).map(_.group(1)).toList
    var partRequired  = new mutable.HashMap[String, Boolean]()
    requiredPaths.foreach { p =>
      partRequired += (p -> true)
    }
    url.validPaths.map(_.path).foreach {
      case s: String if s.contains("{") =>
        if (s.length > path.length) {
          requiredPaths = parts.findAllMatchIn(s).map(_.group(1)).toList
          requiredPaths.foreach { p =>
            if (!partRequired.contains(p)) partRequired += (p -> false)
          }
          path = s
        }
      case value =>
    }
    this.path = path
    val requiredPathsCount = requiredPaths.count(p => partRequired(p))
    val hasBody            = this.body.isDefined
    val bodyType = this.body match {
      case None => "JsonObject"
      case Some(b) =>
        b.serialize match {
          case ""     => "JsonObject"
          case "bulk" => "list"
        }
    }

    this.params.foreach { case (name, param) =>
      parameters += param.toCallParameter(name)
    }

    var bodyDone = false
    if (requiredPaths.nonEmpty) {
      if (requiredPathsCount == 0 && hasBody) {
        parameters += CallParameter(
          "body",
          bodyType,
          "body the body of the call",
          required = this.body.get.required,
          scope = "body"
        )
        bodyDone = true
      }
      parameters ++= requiredPaths.map { p =>
        val partDocumentation = url.getPartDocumentation(p)
        val multiple          = CommonStrings.isMultiple(partDocumentation)
        CallParameter(
          cookPart(p, multiple),
          "string",
          description = partDocumentation,
          required = partRequired(p),
          scope = "uri"
        )
      }.toList

      if (requiredPathsCount > 0 && hasBody) {
        parameters += CallParameter(
          "body",
          bodyType,
          "body the body of the call",
          required = this.body.get.required,
          scope = "body"
        )
        bodyDone = true
      }
    }

    if (!bodyDone && hasBody) {
      parameters += CallParameter(
        "body",
        bodyType,
        "body the body of the call",
        required = this.body.get.required,
        scope = "body"
      )
      bodyDone = true
    }
    parameters ++= this.url.params.filterNot { case (name, value) =>
      requiredPaths.contains(name)
    }.map { case (name, value) =>
      CallParameter(
        name,
        value.`type`,
        description = value.description,
        options = value.options,
        default = value.default,
        required = false,
        subtype = value.subtype
      )

    }.toList

    result match {
      case Some(value) =>
        implicits += value.scala
      case _ =>
    }
    this.implicits = implicits.toList
    this.extra = extra.toMap
    //cleaning dupped names
    val para2 = new mutable.ListBuffer[CallParameter]
    val mset  = new mutable.HashSet[String]
    parameters.foreach { p =>
      if (!mset.contains(p.parameterName)) {
        para2 += p
        mset += p.parameterName
      }
    }

    this.parameters = para2.toList.filter(_.required) ++ para2.filterNot(_.required).sortBy(_.name.dropWhile(_ == '_'))
  }

  def methods: List[String] = url.paths.flatMap(_.methods)

  def toGoodName(value: String): String = value match {
    case value: String if value.contains("_") =>
      val pos = value.indexOf('_')
      value.take(pos) + toGoodName(value.drop(pos + 1).capitalize)
    case _ => value
  }

  private def cookPart(name: String, multiple: Boolean) = name match {
    case "type"  => if (multiple) "docTypes" else "docType"
    case "index" => if (multiple) "indices" else "index"
    case v       => v
  }

  val hasBody: Boolean = this.body.isDefined

  def clientName: String = if (scope == "client") "this" else "client"

  def getClientZIOAccessorsCalls: String = {
    val text     = new ListBuffer[String]
    val funcName = toGoodName(name.split("\\.").last)
    //generating class documentation
    text ++= cookedDocumentation.map(s => "  " + s)

    val defFunc = s"def $funcName("
    text += defFunc

    text += this.parameters.map { parameter =>
      s"${parameter.getDefParameterNoVar}"

    }.mkString(",\n")
    text += "): " + s"ZIO[%%SERVICE%%, FrameworkException, ${getRequestReturn}]"
    text += s" =ZIO.accessM[%%SERVICE%%](_.get.$funcName("

    text += parameters.map { p =>
      val pname = p.parameterName
      s"$pname = $pname"
    }.mkString(",\n")
    text += "))\n\n"

    text += s"  def $funcName(request:${scalaRequest}): ZIO[%%SERVICE%%, FrameworkException, ${getRequestReturn}]= ZIO.accessM[%%SERVICE%%](_.get.execute(request))\n\n"

    text.mkString
  }

  def getClientCalls: String = {
    val text     = new ListBuffer[String]
    val funcName = toGoodName(name.split("\\.").last)
    //generating class documentation
    text ++= cookedDocumentation.map(s => "  " + s)

    val defFunc = s"def $funcName("
    text += defFunc

    text += this.parameters.map { parameter =>
      s"${parameter.getDefParameterNoVar}"

    }.mkString(",\n")
    text += "): " + getRequestZioReturn
    text += " ={\n"

    text += s"val request= ${scalaRequest}("

    text += parameters.map { p =>
      val pname = p.parameterName
      s"$pname = $pname"
    }.mkString(",\n")
    text += ")\n"

    text += "\n"
    text += s"$funcName(request)\n"
    text += "\n  }\n\n"

    text += s"  def $funcName(request:${scalaRequest}):$getRequestZioReturn= $clientName.execute(request)\n\n"

    text.mkString
  }

  def getClientNativeCalls: String = {
    val text     = new ListBuffer[String]
    val funcName = toGoodName(name.split("\\.").last)
    //generating class documentation
    text ++= cookedDocumentation.map(s => "  " + s)

    val defFunc = s"  def $funcName("
    text += defFunc + "\n"

    val newLineFunc: String = " " * defFunc.length

    text += this.parameters.map { parameter =>
      newLineFunc + s"${parameter.getDefParameterNoVar}"

    }.toList.mkString(",\n")
    text += "): " + getRequestZioReturn
    text += " ={\n"

    text += s"val request= new ${scalaRequest}()\n"

    //    val funcCall = s"    $funcName(new $scalaRequest("
    //    var size = funcCall.length

    text += parameters.map { p =>
      val pname = p.parameterName
      if (!p.required) {
        if (p.default.isDefined) {
          s"if(${pname} != ${p.getCookedDefault}})request.${pname}(${pname})"
        } else {
          s"${pname}.foreach{p=> request.${pname}(p) }"
        }
      } else {
        s"request.${pname}(${pname})"
      }
    }.mkString("\n")
    text += "\n"

    text += s"$funcName(request)\n"

    text += "\n  }\n\n"

    text += s"  def $funcName(request:${scalaRequest}):$getRequestZioReturn= ${clientName}.execute(request)\n\n"

    text.mkString
  }

  def requestFilename: String = requestPackage + "." + nativeRequest.get.split('.').last

  def requestPackage: String = s"${Constants.namespaceName}.requests.${scope.replace("client", "")}".stripSuffix(".")

  def responseFilename: String = s"${responsePackage}.${scalaResponse}"
  def responsePackage: String  = s"${Constants.namespaceName}.responses.${scope.replace("client", "")}".stripSuffix(".")

  def responseClass: String = {
    val text = new ListBuffer[String]
    //generating class documentation
    text += s"package $responsePackage\n\n"
    text += s"import scala.collection.mutable\n"
    text += s"import io.circe.derivation.annotations._\n"
    text ++= extra.map(v => s"import ${Constants.namespaceName}.${v._1}\n")

    text ++= cookedDocumentation.map(s => "  " + s)

    text += s"@JsonCodec\n"
    text += s"final case class $scalaResponse(){\n"
    text += "}\n"

    text.mkString
  }

  def getClientCalls_old: String = {
    val text     = new ListBuffer[String]
    val funcName = toGoodName(name.split("\\.").last)
    //generating class documentation
    text ++= cookedDocumentation.map(s => "  " + s)

    val defFunc = s"  def $funcName("
    text += defFunc + "\n"

    val newLineFunc: String = " " * defFunc.length

    text += this.parameters.map { parameter =>
      newLineFunc + s"${parameter.getDefParameterNoVar}"

    }.toList.mkString(",\n")
    text += "): " + getRequestZioReturn
    text += " ={\n"

    text += "    // Custom Code On\n"
    text += "    // Custom Code Off\n"

    val funcCall = s"    $funcName(new $scalaRequest("
    var size     = funcCall.length
    text += funcCall + parameters.map { p =>
      val pname = p.parameterName
      size = size + pname.length * 2 + 1
      if (size > 80) {
        size = pname.length * 2 + 1 + funcCall.length
        "\n" + " " * funcCall.length + s"${pname}=${pname}"
      } else
        s"${pname}=${pname}"
    }.mkString(", ")
    text += s"))\n"

    text += "\n  }\n\n"

    text += s"  def $funcName(request:$scalaRequest):$getRequestZioReturn= ${clientName}.doCall(request).map(fromJsonOrError[$getRequestReturn])\n\n"

    text.mkString
  }

  def getRequestZioReturn: String = s"ZioResponse[${getRequestReturn}]"

  def getRequestReturn: String =
    if (nativeResponse.isDefined)
      nativeResponse.get.split("""\.""").last
    else
      result match {
        case Some(value) => value.scala
        case _           => scalaResponse
      }

  lazy val cookedDocumentation: List[String] = {
    val doc = new ListBuffer[String]
    doc += "/*\n"
    doc += " * " + this.documentation.description + "\n"
    doc += " * For more info refers to " + this.documentation.url + "\n"
    doc += " * \n"
    doc += this.parameters.map(_.getCookedDocumentation).mkString("\n")
    doc += "\n"
    doc += " */\n"
    doc.toList
  }

  def cookdoc(lines: List[String]): String = {
    val text = new ListBuffer[String]
    text += "  /*\n"
    lines.foreach(l => text += "   * " + l.stripMargin(' '))
    text += "   * */"

    text.toList.mkString + "\n"
  }

  lazy val scope: String = {
    val tokens = name.split("\\.")
    var scope  = "client"
    if (tokens.length > 1)
      scope = tokens(tokens.length - 2)
    scope
  }

  def generateRequest: List[String] = {
    val doc = new ListBuffer[String]
    //generating class documentation

    doc ++= cookedDocumentation
    //    doc += "\n"
    val classDef =
      doc += "@JsonCodec\n"
    doc += s"final case class ${scalaRequest}(\n"

    val newLineFunc: String = " " * classDef.length

    doc += this.parameters.map { parameter =>
      val key = if (parameter.name != parameter.parameterName) {
        s"""@JsonKey("${parameter.name}") """
      } else ""
      newLineFunc + s"$key${parameter.getDefParameterNoVar}"
    }.toList.mkString(",\n") + ") extends ActionRequest {\n"
    doc += s"""  def method:String="${methods.head}"\n\n"""
    doc ++= getDefUrlPath
    doc += "\n"
    doc ++= getDefQueryArgs
    doc += "\n"
    if (!hasBody) {
      doc += """  def body:Json=JsNull"""
      doc += "\n"
      doc += "\n"
    }
    doc += "  // Custom Code On\n"
    doc += "  // Custom Code Off\n"
    doc += "\n"

    doc += "}\n"
    doc += "\n"

    doc.toList
  }

  def getDefQueryArgs: List[String] = {
    val parameters = this.parameters.filterNot(_.required).filterNot(_.scope == "uri")
    if (parameters.nonEmpty) {
      val text = new ListBuffer[String]
      text += "  def queryArgs:Map[String, String] = {\n"
      text += "    //managing parameters\n"
      text += "    val queryArgs = new mutable.HashMap[String, String]()\n"
      parameters.foreach { parameter =>
        val code = parameter.toBodyCode
        if (!code.isEmpty)
          text += code
      }
      text += "    // Custom Code On\n"
      text += "    // Custom Code Off\n"

      text += "    queryArgs.toMap\n"
      text += "  }\n"
      text.toList
    } else {
      List("def queryArgs: Map[String, String] = Map.empty[String, String]\n")
    }

  }

  def getDefUrlPath: List[String] = {
    val text = new ListBuffer[String]
    if (this.path.contains("{")) {
      text += s"""  def urlPath:String = this.makeUrl("""
      text += path
        .split("/")
        .map { p =>
          val cleanValue = p.stripPrefix("{").stripSuffix("}")
          if (p.startsWith("{"))
            toGoodName(cookPart(cleanValue, CommonStrings.isMultiple(url.getPartDocumentation(cleanValue))))
          else "\"" + cookPart(p, false) + "\"" //is not a variable it cannot be multiple
        }
        .toList
        .tail
        .mkString(", ")
      text += """)"""
    } else {
      text += s"""  def urlPath = "${this.path}" """
    }
    text += "\n"
    text.toList
  }

  lazy val camelName: String = {
    val name = this.name.replace("_", ".").split("\\.").map(p => p.capitalize).mkString("")
    APIEntry.mappingNames.getOrElse(name, name)
  }

}

object APIEntry {
  val mappingNames: Map[String, String] = Map(
    "Mget"         -> "MultiGet",
    "Mlt"          -> "MoreLikeThis",
    "Mpercolate"   -> "MultiPercolate",
    "Msearch"      -> "MultiSearch",
    "Mtermvectors" -> "MultiTermVectors"
  )

  def processFile(name: File): Either[io.circe.Error, Seq[APIEntry]] =
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
