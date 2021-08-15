package io.megl.generators.scala

import better.files.File
import io.megl.common.PathUtils
import io.megl.common.StringUtils.underscoreToCamel
import org.scalablytyped.converter.internal.ts._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait MetaObject {
  def tsFile: File

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

  def renderBody():String

  def renderJsonSerialization():String={
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

  def populateImports(str: String): Unit ={
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
        case "Names"|"Time" =>
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
      case TsTypeObject(comments, members) => "AnyRef"
      case TsTypeFunction(signature) => "AnyRef"
      case TsTypeConstructor(isAbstract, signature) => "AnyRef"
      case TsTypeIs(ident, tpe) => "AnyRef"
      case TsTypeAsserts(ident, isOpt) => "AnyRef"
      case TsTypeTuple(elems) => "AnyRef"
      case TsTypeQuery(expr) => "AnyRef"
      case TsTypeRepeated(underlying) => "AnyRef"
      case TsTypeKeyOf(key) => "AnyRef"
      case TsTypeLookup(from, key) => "AnyRef"
      case TsTypeThis() => "this"
      case TsTypeIntersect(types) => "AnyRef"
      case TsTypeUnion(types) =>
        val unionTypes = types.map(t => renderType(t)).toList
        unionTypes.length match {
          case 1 =>
            unionTypes.head.typeToScala
          case 2 if unionTypes.contains("None") =>
            s"Option[${unionTypes.filterNot(_ == "None").head.typeToScala}]"
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
              throw new RuntimeException(s"invalid types:$unionTypes")
            }
        }
      case predicate: TsTypePredicate => "AnyRef"
      case TsTypeConditional(pred, ifTrue, ifFalse) => "AnyRef"
      case TsTypeExtends(tpe, ext) => "AnyRef"
      case TsTypeInfer(tparam) => "AnyRef"
    }
}

final case class MetaClass(tsFile: File, name: String, decl: TsDeclClass) extends MetaObject {


  val needJSON: Boolean = true

  def renderBody(): String = {
    val code = new ListBuffer[String]()
    if (decl.isAbstract) {
      code += s"abstract class ${decl.name.value}("
    } else {
      code += s"final case class ${decl.name.value}("
    }

    val members = new ListBuffer[String]()

    decl.members.foreach {
      member =>
        member match {
          case TsMemberCall(comments, level, signature) =>
          case TsMemberCtor(comments, level, signature) =>
          case TsMemberFunction(comments, level, name, methodType, signature, isStatic, isReadOnly) =>
          case TsMemberIndex(comments, isReadOnly, level, indexing, valueType) =>
          case TsMemberTypeMapped(comments, level, readonly, key, from, optionalize, to) =>
          case TsMemberProperty(comments, level, name, tpe, expr, isStatic, isReadOnly) =>
            val realName = name.value
            val varName = underscoreToCamel(realName).nameToMember
            val typ = renderType(tpe.get)
            if (realName != varName) {
              imports += "zio.json._"
              members += s"""@jsonField("$realName") $varName: $typ"""
            } else
              members += s"$realName: $typ"
        }

    }

    code += members.mkString(", ")

    code += ")"

    code.mkString("")
  }
}

final case class MetaEnum(tsFile: File, name: String, enum: TsDeclEnum) extends MetaObject {

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
          case TsExpr.Call(function, params) =>
          case TsExpr.Unary(op, expr) =>
          case TsExpr.BinaryOp(one, op, two) =>
          case TsExpr.Cast(expr, tpe) =>
          case TsExpr.ArrayOf(expr) =>
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
        enumEntry.expr.get match {
          case TsExpr.Ref(value) =>
          case TsExpr.Literal(value) =>
            value match {
              case TsLiteral.Num(v) => code += v
              case TsLiteral.Str(v) => code += "\"" + v + "\""
              case TsLiteral.Bool(v) => code += v.toString
            }
          case TsExpr.Call(function, params) =>
          case TsExpr.Unary(op, expr) =>
          case TsExpr.BinaryOp(one, op, two) =>
          case TsExpr.Cast(expr, tpe) =>
          case TsExpr.ArrayOf(expr) =>
        }
        code += s")\n"
    }
    code += "val values = findValues\n"
    code += "}\n"

    code.mkString("")
  }

  // TODO implement
  override def renderJsonSerialization():String=""
}

final case class MetaInterface(tsFile: File, name: String, interface: TsDeclInterface) extends MetaObject {
  override def renderJsonSerialization():String=""

  def renderBody(): String = {
    val code = new ListBuffer[String]()
    code += s"trait ${interface.name.value}"
    if (interface.tparams.nonEmpty) {
      code += "[" + interface.tparams.map(p => p.name.value).mkString(", ") + "]"
    }
    code += "{"

    val members = new ListBuffer[String]()

    interface.members.foreach {
      member =>
        member match {
          case TsMemberCall(comments, level, signature) =>
          case TsMemberCtor(comments, level, signature) =>
          case TsMemberFunction(comments, level, name, methodType, signature, isStatic, isReadOnly) =>
          case TsMemberIndex(comments, isReadOnly, level, indexing, valueType) =>
          case TsMemberTypeMapped(comments, level, readonly, key, from, optionalize, to) =>
          case TsMemberProperty(comments, level, name, tpe, expr, isStatic, isReadOnly) =>
            val realName = name.value
            val varName = underscoreToCamel(realName).nameToMember
            val typ = renderType(tpe.get)
            if (realName != varName) {
              members +=
                s"""// $realName
                   |def $varName: $typ""".stripMargin
            } else
              members += s"def $realName: $typ"
        }

    }

    code += members.mkString("\n")

    code += "}"

    code.mkString("")
  }
}
