package io.megl.parsers

import better.files.File
import io.megl.generators.scalagen.{MetaClass, MetaEnum, MetaInterface, ScalaMetaObject}
import org.scalablytyped.converter.internal.ts._

case class TSFileMetadata(file: File, decl: TsNamedDecl, name: String, depends: List[String]) {
  var isTrait                           = decl.isInstanceOf[TsDeclInterface]
  lazy val isContainer: Boolean         = file.name.endsWith("Container.ts")
  var container: Option[TSFileMetadata] = None
  var hintName: Option[String]          = None
  var shouldRender: Boolean= true

  lazy val scalaMetaObject=convertToClass(this)

  private def convertToClass(instance:TSFileMetadata): Option[ScalaMetaObject] = {
    instance.decl match {
      case _:TsDeclNamespace => None
      case c: TsDeclClass =>
        Some(MetaClass(instance, c.name.value, c))

      case c: TsDeclInterface =>
        Some(MetaInterface(instance, c.name.value, c))
      case c: TsDeclEnum =>
        Some(MetaEnum(instance, c.name.value, c))

      case _:TsDeclVar => None
      case _:TsDeclFunction => None
      case _:TsDeclTypeAlias => None
      case _: TsNamedValueDecl => None
    }
  }

  lazy val fields: List[MemberDescriptor] = {
    decl match {
      case c: TsDeclClass     => c.members.toList.flatMap(toFieldDescriptor)
      case c: TsDeclInterface => c.members.toList.flatMap(toFieldDescriptor)
      case _                  => Nil
    }
  }

  def toFieldDescriptor(member: TsMember): Option[MemberDescriptor] =
    member match {
      case _: TsMemberCall       => None
      case _: TsMemberCtor       => None
      case _: TsMemberFunction   => None
      case _: TsMemberIndex      => None
      case _: TsMemberTypeMapped => None
      case TsMemberProperty(_, _, name, tpe, _, _, _) =>
        Some(MemberDescriptor(member, name.value, tpe.get))
    }

}

object TSFileMetadata {
  def extractName(tsType: TsType): String = tsType match {
    case TsTypeRef(_, name, _) => name.parts.head.value
    case _                     => tsType.asString
  }

  def apply(file: File, cls: TsDeclClass): TSFileMetadata = new TSFileMetadata(
    file,
    cls,
    cls.name.value,
    cls.parent.toList.map(extractName) ++ cls.implements.toList.map(extractName)
  )

  def apply(file: File, cls: TsDeclInterface): TSFileMetadata =
    new TSFileMetadata(file, cls, cls.name.value, cls.inheritance.toList.map(extractName))

  def apply(file: File, cls: TsDeclEnum): TSFileMetadata = new TSFileMetadata(file, cls, cls.name.value, Nil)

}
