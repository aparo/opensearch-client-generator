package io.megl.parsers

import org.scalablytyped.converter.internal.ts._

final case class MemberDescriptor(
  decl: TsMember,
  name: String,
  tpe: TsType,
  typeName: String,
  multiple: Boolean,
  nullable: Boolean
) {}

object MemberDescriptor {
  def apply(decl: TsMember, name: String, tpe: TsType): MemberDescriptor = {
    val (typeName, multiple, nullable) = getType(tpe)
    MemberDescriptor(decl, name, tpe, typeName, multiple, nullable)
  }

  lazy val undefined: String = "undefined"

  def renderType(value: TsType): String =
    value match {
      case TsTypeRef(comments, name, tparams) =>
        name.parts.length match {
          case 1 =>
            var n = name.parts.head.value
            if (tparams.nonEmpty)
              n += "[" + tparams.map(v => renderType(v)).mkString(", ") + "]"
            n
        }
      case TsTypeLiteral(literal) => literal.asString
      case _: TsTypeObject        => "Any"
      case _: TsTypeFunction      => "Any"
      case _: TsTypeConstructor   => "Any"
      case _: TsTypeIs            => "Any"
      case _: TsTypeAsserts       => "Any"
      case _: TsTypeTuple         => "Any"
      case _: TsTypeQuery         => "Any"
      case _: TsTypeRepeated      => "Any"
      case _: TsTypeKeyOf         => "Any"
      case _: TsTypeLookup        => "Any"
      case _: TsTypeThis          => "this"
      case _: TsTypeIntersect     => "Any"
      case TsTypeUnion(types) =>
        val unionTypes = types.map(t => renderType(t)).toList
        val optional   = unionTypes.contains(undefined)
        unionTypes.length match {
          case 1 =>
            unionTypes.head
          case 2 if unionTypes.contains(undefined) =>
            s"Option[${unionTypes.filterNot(_ == undefined).head}]"
          case 2 =>
            s"Either[${unionTypes.head},${unionTypes.last}]"
          case 3 =>
            val items = unionTypes
            if (items.contains("string") && items.contains("Array[string]")) {
              if (items.contains(undefined)) {
                "Option[StringOrArray]"
              } else {
                throw new RuntimeException(s"invalid types:$unionTypes")
              }
            } else {

              //throw new RuntimeException(s"invalid types:$unionTypes")
              "zio.json.ast.Json"
            }
          case _ =>
            val types = unionTypes.filterNot(_ == undefined).mkString
            if (optional) {
              s"Option[$types]"
            } else types
        }
      case predicate: TsTypePredicate               => "Any"
      case TsTypeConditional(pred, ifTrue, ifFalse) => "Any"
      case TsTypeExtends(tpe, ext)                  => "Any"
      case TsTypeInfer(tparam)                      => "Any"
    }

  def getType(value: TsType): (String, Boolean, Boolean) =
    value match {
      case TsTypeRef(comments, name, tparams) =>
        name.parts.length match {
          case 1 =>
            var n        = name.parts.head.value
            var nullable = false
            var multiple = false
            if (n == "Array")
              multiple = true
            if (tparams.nonEmpty)
              n += "[" + tparams.map(v => renderType(v)).mkString(", ") + "]"

            (n, multiple, nullable)
        }
      case TsTypeLiteral(literal) => (literal.asString, false, false)
      case _: TsTypeObject        => ("Any", false, false)
      case _: TsTypeFunction      => ("Any", false, false)
      case _: TsTypeConstructor   => ("Any", false, false)
      case _: TsTypeIs            => ("Any", false, false)
      case _: TsTypeAsserts       => ("Any", false, false)
      case _: TsTypeTuple         => ("Any", false, false)
      case _: TsTypeQuery         => ("Any", false, false)
      case _: TsTypeRepeated      => ("Any", false, false)
      case _: TsTypeKeyOf         => ("Any", false, false)
      case _: TsTypeLookup        => ("Any", false, false)
      case _: TsTypeThis          => ("this", false, false)
      case _: TsTypeIntersect     => ("Any", false, false)
      case TsTypeUnion(types) =>
        val unionTypesWithNull = types.map(t => renderType(t)).toList
        val nullable           = unionTypesWithNull.contains(undefined)
        val unionTypes         = unionTypesWithNull.filterNot(_ == undefined)
        unionTypes.length match {
          case 1 =>
            (unionTypes.head, false, nullable)
          case 2 =>
            (s"Either[${unionTypes.head},${unionTypes.last}]", false, nullable)
          case 3 =>
            val items = unionTypes
            //throw new RuntimeException(s"invalid types:$unionTypes")
            ("zio.json.ast.Json", false, nullable)
          case _ =>
            val types = unionTypes.filterNot(_ == undefined).mkString
            if (nullable) {
              ("$types", false, nullable)
            } else (types, false, nullable)
        }
      case predicate: TsTypePredicate               => ("Any", false, false)
      case TsTypeConditional(pred, ifTrue, ifFalse) => ("Any", false, false)
      case TsTypeExtends(tpe, ext)                  => ("Any", false, false)
      case TsTypeInfer(tparam)                      => ("Any", false, false)
    }

}
