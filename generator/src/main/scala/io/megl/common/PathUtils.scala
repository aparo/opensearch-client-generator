/*
 * Copyright 2021 Alberto Paro
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.megl.common

import scala.jdk.CollectionConverters._
import scala.util.Try

import better.files._
import com.github.difflib.DiffUtils
import com.github.difflib.patch.DeltaType

/**
 * Common functionalities for working with the paths
 */
object PathUtils {

  /**
   * Find a file in this path of recurvicely in subpaths
   * @param startPath startPath
   * @param name name of the file
   * @return if the faile is found
   */
  def findFile(name: String, startPath: File = File.currentWorkingDirectory): Option[File] = {
    val curr = startPath / name
    if (curr.exists)
      Some(curr)
    else
      startPath.parentOption match {
        case Some(value) => findFile(name, value)
        case None        => None
      }
  }

//  def recursivelyListFiles(f: File): Array[File] = {
//    val these = f.listFiles
//    these ++ these.filter(_.isDirectory).flatMap(recursivelyListFiles)
//  }

  def getScala(paths: Array[String]): List[File] =
    paths.flatMap((dir) => File(dir).listRecursively).filter(_.name.endsWith(".scala")).toList

  def expandFiles(list: List[better.files.File]): List[better.files.File] =
    expandFiles(list, _ => true, Set("target", "tmp", ".metals", ".bloop"))

  def expandFiles(
    list: List[better.files.File],
    filter: better.files.File => Boolean,
    invalidDirectories: Set[String]
  ): List[better.files.File] =
    list.flatMap { file =>
      if (file.isDirectory)
        file.listRecursively.toList
          .filterNot(f => invalidDirectories.exists(d => f.toString().contains(d)))
          .filter(_.isRegularFile)
          .filter(filter)
      else if (file.isRegularFile && filter(file)) {
        Nil
      } else List(file).filter(filter)
    }

  def updateFileIfNeeded(file: File, content: String): Unit = {
    if (file.exists) {
      val oldContent = file.contentAsString
      if (oldContent != content) {
        file.writeText(content)
      }
    } else {
      file.parent.createDirectoryIfNotExists()
      file.writeText(content)
    }
    ()
  }

  def save(file: File, content: String, templateFlags: List[String] = Nil): Unit =
    if (file.exists && templateFlags.nonEmpty) {
      val result = mergeTemplate(
        file.contentAsString.split('\n').toList,
        content.split('\n').toList,
        templateFlags = templateFlags
      )
      PathUtils.updateFileIfNeeded(file, result.mkString("\n"))
    } else {
      PathUtils.updateFileIfNeeded(file, content)
    }

  def mergeTemplate(
    original: List[String],
    generated: List[String],
    templateFlags: List[String] = Nil
  ): List[String] = {

    val patch  = DiffUtils.diff(original.asJava, generated.asJava)
    var result = original

    //simple output the computed patch to console
    val deltas  = patch.getDeltas.asScala.reverse.toList
    var nextpos = 0
    deltas.foreach { delta =>
      delta.getType match {
        case DeltaType.CHANGE =>
          val position = delta.getSource.getPosition
          result = result.take(position) ++ delta.getTarget.getLines.asScala ++ result.drop(position)
        case DeltaType.DELETE =>
          val position = delta.getSource.getPosition
          // check sources
          val lines      = delta.getSource.getLines.asScala.toList
          var inTemplate = false
          if (lines.nonEmpty) {
            val h = lines.head
            if (templateFlags.contains(h)) {
              inTemplate = true
              nextpos = position + lines.length
            }
          }
          if (position == nextpos + 1) {
            inTemplate = true
          }

          if (!inTemplate) {
            result = result.take(position) ++ result.drop(position + lines.length)
          } else {
            // if we are in template we skip removing
          }
        case DeltaType.INSERT =>
          val position = delta.getSource.getPosition
          //          if(templateFlags.isEmpty){
          // all new code is inderted
          result = result.take(position) ++ delta.getTarget.getLines.asScala ++ result.drop(position)
        //          }

        case DeltaType.EQUAL =>
      }
    }
    result
  }

  /**
   * Save a given content in a file if the content is different
   * @param content the content to be saved
   * @param file the file to be saved
   */
  def saveScalaFile(content: String, file: File): String = {
    val result = Try {
      org.scalafmt.Scalafmt.format(content).get
    }.toOption.getOrElse(content)
    PathUtils.updateFileIfNeeded(file, result)
    //    logger.info(s"Generated $file")
    result
  }

}
