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

  def render(): String

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


  implicit class ScalaConversions(str: String) {
    def typeToScala: String = {
      str match {
        case "string" => "String"
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


  def renderType(tpe: Option[TsType]): String = {
    tpe match {
      case Some(value) =>
        value match {
          case TsTypeRef(comments, name, tparams) => name.parts.length match {
            case 1 => name.parts.head.value
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
            val unionTypes = types.map(t => renderType(Some(t))).toList
            unionTypes.length match {
              case 1 => unionTypes.head
              case 2 if unionTypes(1) == "undefined" => s"Option[${unionTypes.head.typeToScala}]"
            }
          case predicate: TsTypePredicate => "AnyRef"
          case TsTypeConditional(pred, ifTrue, ifFalse) => "AnyRef"
          case TsTypeExtends(tpe, ext) => "AnyRef"
          case TsTypeInfer(tparam) => "AnyRef"
        }
      case None => "AnyRef"
    }
  }

}

final case class MetaClass(tsFile: File, name: String, decl: TsDeclClass) extends MetaObject {

  private val imports=new mutable.HashSet[String]()

  override def render(): String = {
    val code = new ListBuffer[String]()
    code += s"package $packageName\n\n"
    val body = renderBody()

    imports.toList.sorted.foreach(imp => code += s"import $imp\n")
    code += "\n"

    code += body
    code.mkString
  }

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
            if (realName != varName) {
              imports+="zio.json._"
              members += s"""@jsonField("$realName") $varName: ${renderType(tpe)}"""
            } else
              members += s"$realName: ${renderType(tpe)}"
        }

    }

    code += members.mkString(", ")

    code += ")"

    code.mkString("")
  }
}

final case class MetaEnum(tsFile: File, name: String, enum: TsDeclEnum) extends MetaObject {


  def render(): String = {
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

}
