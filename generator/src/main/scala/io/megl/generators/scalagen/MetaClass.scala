package io.megl.generators.scalagen

import better.files.File
import io.megl.common.PathUtils
import io.megl.common.StringUtils.underscoreToCamel
import io.megl.parsers.TSFileMetadata
import org.scalablytyped.converter.internal.ts._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait ScalaMetaObject {
  def tsFile: TSFileMetadata

  def name: String

  protected val imports = new mutable.HashSet[String]()

  def render(): String = {
    val code = new ListBuffer[String]()
    code += s"package $packageName\n\n"
    val body = renderBody()
    val jsonCode = renderJsonSerialization()
    populateImports(body)

    imports.toList.sorted.foreach(imp => code += s"import $imp\n")
    code += "\n"

    code += body
    code += jsonCode

    code.mkString
  }

  def renderBody(): String

  def renderJsonSerialization(): String = {
    imports += "zio.json._"
    s"""
       |object $name {
       |  implicit val codec: JsonCodec[$name] = DeriveJsonCodec.gen[$name]
       |}
       |""".stripMargin
  }

  def save(destination: File): Unit = {
    PathUtils.saveScalaFile(
      render(),
      destination / packageName.replace(".", "/") / s"$name.scala"
    )
  }

  lazy val packageName: String = {
    val tokens = tsFile.toString().substring(tsFile.toString().indexOf("/specs/") + "/specs/".length).split('/')
    val parts = List("opensearch") ++ tokens.init.toList
    parts.mkString(".")
  }

  def populateImports(str: String): Unit = {
  }

  implicit class ScalaConversions(str: String) {
    def typeToScala: String = {
      str match {
        case "string" => "String"
        case "boolean" => "Boolean"
        case "undefined" => "None"
        case "short" => "Short"
        case "byte" => "Byte"
        case "integer" => "Int"
        case "uint" => "Int"
        case "long" => "Long"
        case "ulong" => "Long"
        case "float" => "Float"
        case "double" => "Double"
        case "Names" | "Time" | "LatLon" =>
          imports += "opensearch._"
          str
        case "Dictionary" => "Map" // common.ts

        case "ErrorCause" => "ErrorCauseFormatter" // common.ts
        case "MainError" => "ErrorFormatter" // common.ts
        case "UserDefinedValue" =>
          imports += "zio.json.ast._"
          "Json"

        case _ => str
      }
    }

    def nameToMember: String = {
      if (str.contains(".")) {
        val tokens = str.split('.')
        tokens.head + tokens.tail.map(_.capitalize).mkString
      } else str
    }

  }


  def renderType(value: TsType): String =
    value match {
      case TsTypeRef(comments, name, tparams) => name.parts.length match {
        case 1 =>
          var n = name.parts.head.value.typeToScala
          if (tparams.nonEmpty)
            n += "[" + tparams.map(v => renderType(v)).mkString(", ") + "]"
          n
      }
      case TsTypeLiteral(literal) => literal.asString
      case _:TsTypeObject => "AnyRef"
      case _:TsTypeFunction => "AnyRef"
      case _:TsTypeConstructor => "AnyRef"
      case _:TsTypeIs => "AnyRef"
      case _:TsTypeAsserts => "AnyRef"
      case _:TsTypeTuple => "AnyRef"
      case _:TsTypeQuery => "AnyRef"
      case _:TsTypeRepeated => "AnyRef"
      case _:TsTypeKeyOf => "AnyRef"
      case _:TsTypeLookup => "AnyRef"
      case _:TsTypeThis => "this"
      case _:TsTypeIntersect => "AnyRef"
      case TsTypeUnion(types) =>
        val unionTypes = types.map(t => renderType(t)).toList
        val optional=unionTypes.contains("None")
        unionTypes.length match {
          case 1 =>
            unionTypes.head.typeToScala
          case 2 if unionTypes.contains("None") =>
            s"Option[${unionTypes.filterNot(_ == "None").head.typeToScala}]"
          case 2 =>
            s"Either[${unionTypes.head},${unionTypes.last}]"
          case 3 =>
            val items = unionTypes.map(_.typeToScala)
            if (items.contains("String") && items.contains("Array[String]")) {
              if (items.contains("None")) {
                imports += "opensearch._"
                "Option[StringOrArray]"
              } else {
                throw new RuntimeException(s"invalid types:$unionTypes")
              }
            } else {

              //throw new RuntimeException(s"invalid types:$unionTypes")
              "zio.json.ast.Json"
            }
          case _ =>
            val types=unionTypes.filterNot(_ == "None").mkString
            if (optional){
              s"Option[$types]"
            } else types
        }
      case predicate: TsTypePredicate => "AnyRef"
      case TsTypeConditional(pred, ifTrue, ifFalse) => "AnyRef"
      case TsTypeExtends(tpe, ext) => "AnyRef"
      case TsTypeInfer(tparam) => "AnyRef"
    }

  def memberToCaseClass(member: TsMember): String = {
    member match {
      case _:TsMemberCall => ""
      case _:TsMemberCtor => ""
      case _:TsMemberFunction => ""
      case _:TsMemberIndex => ""
      case _:TsMemberTypeMapped => ""
      case TsMemberProperty(comments, level, name, tpe, expr, isStatic, isReadOnly) =>
        val realName = name.value
        val varName = underscoreToCamel(realName).nameToMember
        var typ = renderType(tpe.get)
        val default = typ match {
          case s: String if s.startsWith("Option[Map[") =>
            typ = typ.substring("Option[".length).init
            "Map.empty" + typ.drop(3)
          case s: String if s.startsWith("Option[") =>
            "None"
          case s: String if s.startsWith("List[") =>
            typ = typ.substring("List[".length).init
            "Nil"
          case _ => ""
        }
        if (realName != varName) {
          imports += "zio.json._"
          if (default.nonEmpty)
            s"""@jsonField("$realName") $varName: $typ = $default"""
          else
            s"""@jsonField("$realName") $varName: $typ"""
        } else if (default.nonEmpty)
          s"$realName: $typ = $default"
        else
          s"$realName: $typ"

    }
  }

  def memberToCaseMethod(member: TsMember): String = {
    member match {
      case _:TsMemberCall => ""
      case _:TsMemberCtor => ""
      case _:TsMemberFunction => ""
      case _:TsMemberIndex => ""
      case _:TsMemberTypeMapped => ""
      case TsMemberProperty(comments, level, name, tpe, expr, isStatic, isReadOnly) =>
        val realName = name.value
        val varName = underscoreToCamel(realName).nameToMember
        val typ = renderType(tpe.get)
        if (realName != varName) {
          s"""// $realName
             |def $varName: $typ""".stripMargin
        } else
          s"def $realName: $typ"
    }
  }
}

final case class MetaClass(tsFile: TSFileMetadata, name: String, cls: TsDeclClass) extends ScalaMetaObject {

  val needJSON: Boolean = true

  def getParents(): String = {
    val parents = cls.parent.toList.map(renderType) ++ cls.implements.toList.map(renderType)
    if (parents.nonEmpty) {
      s" extends ${parents.head}" + parents.tail.map(v => s"with $v").mkString
    } else ""
  }

  def renderBody(): String = {
    val code = new ListBuffer[String]()
    val parents = getParents()
    if (tsFile.isTrait) {
      code += s"trait ${cls.name.value} $parents{"
      code += cls.members.map(memberToCaseMethod).filter(_.nonEmpty).mkString("\n")
      code += "}"
    } else {
      if (cls.isAbstract) {
        code += s"abstract class ${cls.name.value}("
      } else {
        code += s"final case class ${cls.name.value}("
      }
      code += cls.members.map(memberToCaseClass).filter(_.nonEmpty).mkString(", ")
      code += s")$parents"
    }

    code.mkString("")
  }
}

final case class MetaEnum(tsFile: TSFileMetadata, name: String, enum: TsDeclEnum) extends ScalaMetaObject {

  def renderBody(): String = {
    val code = new ListBuffer[String]()
    // we check type of enum
    var enumType = "String"
    enum.members.head.expr.foreach {
      expr =>
        expr match {
          case TsExpr.Ref(value) =>
          case TsExpr.Literal(value) =>
            value match {
              case TsLiteral.Num(vNum) =>
                val intRegex = """(\d+)""".r
                vNum match {
                  case intRegex(str) => str.toInt
                    enumType = "Integer"
                  case _ =>
                    enumType = "Double"
                }

              case TsLiteral.Str(value) =>
                enumType = "String"
              case TsLiteral.Bool(value) =>
                enumType = "Boolean"
            }
          case _:TsExpr.Call =>
          case _:TsExpr.Unary =>
          case _:TsExpr.BinaryOp =>
          case _:TsExpr.Cast =>
          case _:TsExpr.ArrayOf =>
        }
    }

    code += "import enumeratum.values._"
    code += "\n\n"
    code += s"sealed abstract class ${enum.name.value}(val value: Int) extends IntEnumEntry with AllowAlias"
    code += "\n"
    code += s"object ${enum.name.value} extends IntEnum[${enum.name.value}] {\n"
    enum.members.foreach {
      enumEntry =>
        code += s"  object ${enumEntry.name.value} extends ${enum.name.value}("
        enumEntry.expr.foreach{
          case _:TsExpr.Ref =>
          case TsExpr.Literal(value) =>
            value match {
              case TsLiteral.Num(v) => code += v
              case TsLiteral.Str(v) => code += "\"" + v + "\""
              case TsLiteral.Bool(v) => code += v.toString
            }
          case _:TsExpr.Call =>
          case _:TsExpr.Unary =>
          case _:TsExpr.BinaryOp =>
          case _:TsExpr.Cast =>
          case _:TsExpr.ArrayOf =>
        }
        code += s")\n"
    }
    code += "val values = findValues\n"
    code += "}\n"

    code.mkString("")
  }

  // TODO implement
  override def renderJsonSerialization(): String = ""
}

final case class MetaInterface(tsFile: TSFileMetadata, name: String, interface: TsDeclInterface) extends ScalaMetaObject {
  override def renderJsonSerialization(): String = ""

  def getParents(): String = {
    val parents = interface.inheritance.toList.map(renderType)
    if (parents.nonEmpty) {
      s" extends ${parents.head}" + parents.tail.map(v => s"with $v").mkString
    } else ""
  }


  def renderBody(): String = {
    val code = new ListBuffer[String]()
    val parents = getParents()
    code += s"trait ${interface.name.value}"
    if (interface.tparams.nonEmpty) {
      code += "[" + interface.tparams.map(p => p.name.value).mkString(", ") + "]"
    }
    code += s"$parents {"

    code += interface.members.map(memberToCaseMethod).filter(_.nonEmpty).mkString("\n")

    code += "}"

    code.mkString("")
  }
}
