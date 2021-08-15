package io.megl.generators.scala

import better.files.File
import io.megl.generators.{GeneratorContext, GeneratorTrait}
import org.scalablytyped.converter.internal.ts._

class ScalaClassGenerator(generatorContext: GeneratorContext) extends GeneratorTrait {


  override def generate(): Unit = {
    val SAVE_PATH = File("/Users/alberto/opensearch/opensearch-scala-client/core/shared/src/main/scala/")
    generatorContext.entities.foreach {
      case (file, tsObj) =>
        println(tsObj)
        convertToClass(file, tsObj).foreach {
          case metaObject =>
            metaObject.save(SAVE_PATH)
        }

    }

  }

  def convertToClass(file: File, parsed: TsNamedDecl): Option[MetaObject] = {
    parsed match {
      case TsDeclNamespace(comments, declared, name, members, codePath, jsLocation) => None
      case c: TsDeclClass =>
        Some(MetaClass(file, c.name.value, c))

      case TsDeclInterface(comments, declared, name, tparams, inheritance, members, codePath) => None
      case c: TsDeclEnum =>
        Some(MetaEnum(file, c.name.value, c))

      case TsDeclVar(comments, declared, readOnly, name, tpe, expr, jsLocation, codePath) => None
      case TsDeclFunction(comments, declared, name, signature, jsLocation, codePath) => None
      case TsDeclTypeAlias(comments, declared, name, tparams, alias, codePath) => None
      case decl: TsNamedValueDecl => None
    }
  }


  def extractScalaFile(srcFile: File, name: String): File = {
    val tokens = srcFile.toString().substring(srcFile.toString().indexOf("/specs/") + "/specs/".length).split('/')
    val parts = List(s"scala-client", "opensearch") ++ tokens.init.toList ++ List(s"$name.scala")
    File(parts.mkString("/"))
  }

}
