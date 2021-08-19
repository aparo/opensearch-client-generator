package io.megl.parsers

import better.files.File
import org.scalablytyped.converter.internal.ts._

case class TSFileMetadata(file: File, decl: TsNamedDecl, name: String, depends: List[String]){
  var isTrait=decl.isInstanceOf[TsDeclInterface]
}

object TSFileMetadata {
  def extractName(tsType: TsType): String = tsType match {
    case TsTypeRef(_, name, _) => name.parts.head.value
    case _ => tsType.asString
  }

  def apply(file: File, cls: TsDeclClass): TSFileMetadata = new TSFileMetadata(file, cls, cls.name.value, cls.parent.toList.map(extractName) ++ cls.implements.toList.map(extractName))

  def apply(file: File, cls: TsDeclInterface): TSFileMetadata = new TSFileMetadata(file, cls, cls.name.value, cls.inheritance.toList.map(extractName))

  def apply(file: File, cls: TsDeclEnum): TSFileMetadata = new TSFileMetadata(file, cls, cls.name.value, Nil)

}
