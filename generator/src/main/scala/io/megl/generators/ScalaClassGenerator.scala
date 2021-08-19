package io.megl.generators


import better.files.File
import io.megl.generators.scalagen.{MetaClass, MetaEnum, MetaInterface, ScalaMetaObject}
import io.megl.generators.{GeneratorContext, GeneratorTrait}
import io.megl.parsers.TSFileMetadata
import org.scalablytyped.converter.internal.ts._

class ScalaClassGenerator() extends GeneratorTrait {
  GeneratorContext.fixContextForScala()


  override def generate(): Unit = {
    val SAVE_PATH = File("/Users/alberto/opensearch/opensearch-scala-client/core/shared/src/main/scala/")
    GeneratorContext
      .dependencyTree
      .foreach{entry =>
        val instance=GeneratorContext.entities(entry)
        convertToClass(instance).foreach {
          case metaObject =>
            metaObject.save(SAVE_PATH)
        }

      }
    //    generatorContext.entities.foreach {
    //      case (file, tsObj) =>
    //        println(tsObj)
    //        convertToClass(file, tsObj).foreach {
    //          case metaObject =>
    //            metaObject.save(SAVE_PATH)
    //        }
    //
    //    }

  }

  def convertToClass(instance:TSFileMetadata): Option[ScalaMetaObject] = {
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
      case decl: TsNamedValueDecl => None
    }
  }


  def extractScalaFile(srcFile: File, name: String): File = {
    val tokens = srcFile.toString().substring(srcFile.toString().indexOf("/specs/") + "/specs/".length).split('/')
    val parts = List(s"scala-client", "opensearch") ++ tokens.init.toList ++ List(s"$name.scala")
    File(parts.mkString("/"))
  }

}
