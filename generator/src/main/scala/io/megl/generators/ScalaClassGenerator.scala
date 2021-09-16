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
        GeneratorContext.entities.get(entry) match {
          case Some(instance) =>
            if(instance.shouldRender)
              instance.scalaMetaObject.foreach {
                case metaObject =>
                  metaObject.save(SAVE_PATH)
              }
          case None =>
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




  def extractScalaFile(srcFile: File, name: String): File = {
    val tokens = srcFile.toString().substring(srcFile.toString().indexOf("/specs/") + "/specs/".length).split('/')
    val parts = List(s"scala-client", "opensearch") ++ tokens.init.toList ++ List(s"$name.scala")
    File(parts.mkString("/"))
  }

}
