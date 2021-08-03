package io.megl.generators

import better.files.File
import org.scalablytyped.converter.internal.ts._

import scala.collection.mutable.ListBuffer

class ScalaClassGenerator(generatorContext: GeneratorContext) extends GeneratorTrait {
  override def generate(): Unit = {

  }

  def convertToClass(file: File, parsed: TsNamedDecl): Option[(File, String)] = {
    parsed match {
      case TsDeclNamespace(comments, declared, name, members, codePath, jsLocation) => None
      case c: TsDeclClass =>
        Some(file -> renderClass(c))

      case TsDeclInterface(comments, declared, name, tparams, inheritance, members, codePath) => None
      case TsDeclEnum(comments, declared, isConst, name, members, isValue, exportedFrom, jsLocation, codePath) => None
      case TsDeclVar(comments, declared, readOnly, name, tpe, expr, jsLocation, codePath) => None
      case TsDeclFunction(comments, declared, name, signature, jsLocation, codePath) => None
      case TsDeclTypeAlias(comments, declared, name, tparams, alias, codePath) => None
      case decl: TsNamedValueDecl => None
    }
  }

  private def renderClass(cls:TsDeclClass):String={
    val code=new ListBuffer[String]()
    if (cls.isAbstract) {
      code +=s"abstract class ${cls.name.value}("
    } else {
      code +=s"final case class ${cls.name.value}("
    }

    val members=new ListBuffer[String]()

    cls.members.foreach{
        member =>
      member match {
        case TsMemberCall(comments, level, signature) =>
        case TsMemberCtor(comments, level, signature) =>
        case TsMemberFunction(comments, level, name, methodType, signature, isStatic, isReadOnly) =>
        case TsMemberIndex(comments, isReadOnly, level, indexing, valueType) =>
        case TsMemberTypeMapped(comments, level, readonly, key, from, optionalize, to) =>
        case TsMemberProperty(comments, level, name, tpe, expr, isStatic, isReadOnly) =>
          members += s"${name.value}: ${renderType(tpe)}"
      }

    }

    code += members.mkString(", ")

    code +=")"

    code.mkString("")
  }

  def renderType(tpe:Option[TsType]):String={
    tpe match {
      case Some(value) =>
        value match {
          case TsTypeRef(comments, name, tparams) =>  name.parts.length match {
            case 1 => name.parts.head.value
          }
          case TsTypeLiteral(literal) =>literal.asString
          case TsTypeObject(comments, members) =>"AnyRef"
          case TsTypeFunction(signature) =>"AnyRef"
          case TsTypeConstructor(isAbstract, signature) =>"AnyRef"
          case TsTypeIs(ident, tpe) =>"AnyRef"
          case TsTypeAsserts(ident, isOpt) =>"AnyRef"
          case TsTypeTuple(elems) =>"AnyRef"
          case TsTypeQuery(expr) =>"AnyRef"
          case TsTypeRepeated(underlying) => "AnyRef"
          case TsTypeKeyOf(key) =>"AnyRef"
          case TsTypeLookup(from, key) =>"AnyRef"
          case TsTypeThis() =>"this"
          case TsTypeIntersect(types) =>"AnyRef"
          case TsTypeUnion(types) =>
            val unionTypes=types.map(t => renderType(Some(t))).toList
            unionTypes.length match {
              case 1 => unionTypes.head
              case 2 if unionTypes(1)=="undefined" => s"Option[${unionTypes.head}]"
            }
          case predicate: TsTypePredicate =>"AnyRef"
          case TsTypeConditional(pred, ifTrue, ifFalse) =>"AnyRef"
          case TsTypeExtends(tpe, ext) =>"AnyRef"
          case TsTypeInfer(tparam) => "AnyRef"
        }
      case None =>"AnyRef"
    }
  }

}
