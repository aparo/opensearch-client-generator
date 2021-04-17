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

package io.megl.parsers.jsonspec

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import better.files._
import io.megl.Constants
import io.megl.common.PathUtils

class ElasticSearchScalaCodeGenerator extends BaseCodeGenerator {

  var nativeFiltering = false

  val destDir: File = Constants.devScalaAPIDestPath / "elasticsearch"
  destDir.createDirectories()

  lazy val elasticFiles: List[File] =
    Constants.devESSourcePath.listRecursively.filter(_.name.endsWith(".java")).toList

  val requestResponse = new ListBuffer[(String, String)]()

  def run(): Unit =
//    runMappings()
    runREST()
  def runMappings(): Unit = {
    val apis = mappingFiles.flatMap { f =>
      processFile(f) match {
        case Left(value) =>
          println(f)
          println(value)
          None
        case Right(value) =>
          Some(value)
      }
    }
    println(apis)

  }
  def runREST(): Unit = {

    val apis = files
      //      .filter(_.name == "get.json")
      .flatMap { f =>
        processFile(f) match {
          case Left(value) =>
            println(f)
            println(value)
            None
          case Right(value) =>
            Some(value)
        }
      }.flatten.toList.sortBy(_.name)

    val esActions = elasticFiles
      .filter(_.toString().contains("/action/"))
      .filterNot(_.toString().contains("/post/"))
      .filterNot(_.toString().contains("/rest/"))
      .filter(_.name.endsWith("Action.java"))
      .map(_.name.split('.').head)
      .filterNot(_.startsWith("Abstract"))
      .filterNot(_.startsWith("Node"))
      .filterNot(_.endsWith("AsyncAction"))
      .filterNot(_ == "Action")
      .filterNot(_ == "GenericAction")
    val currentActions = apis.flatMap(_.nativeAction)
    esActions.toSet -- currentActions.map(_.split('.').last).toSet

//    val actionToAdd = missing.flatMap { toAdd =>
//      elasticFiles
//        .filter(_.toString().contains("/action/"))
//        .find(f => f.toString().contains(s"$toAdd.java") && !f.toString().contains("Transport"))
//        .map { file =>
//          file.toString().split("/java/").last.replace(".java", "").replace("/", ".")
//        }
//    }
//    actionToAdd.toList.sorted.foreach{
//      actionName=>
//        println(s"""
//      "native_action":"$actionName",
//      "native_request":"${actionName.replace("Action", "Request")}",
//      "native_response":"${actionName.replace("Action", "Response")}",
//        """)
//    }

//    // generate ActionMagner -- no more used
//    val actionMTemplate = elasticsearch.client.txt.ActionMagnet
//    val actions         = apis.flatMap(_.nativeAction).toSet.toList.sorted
//    PathUtils.saveScalaFile(actionMTemplate.render(actions).toString(), destDir / "elasticsearch" / "client"/ "ActionMagnet.scala")

    val zioAccessManagers = new mutable.HashMap[String, String]()

    apis.foreach { apiEntry: APIEntry =>
      // generate Request
      generateRequest(destDir, apiEntry)
      // generate Response
      PathUtils.saveScalaFile(
        apiEntry.responseClass,
        destDir.parent / s"${apiEntry.responseFilename.split("\\.", 1).last.replace(".", "/")}.scala"
      )

      requestResponse += apiEntry.scalaRequest -> apiEntry.scalaResponse
      val client           = apiEntry.scope
      val extra            = apiEntry.extra
      val implicitsList    = apiEntry.implicits
      val code             = apiEntry.getClientCalls
      val zioAccessMethods = apiEntry.getClientZIOAccessorsCalls
      val imports = List(
        s"elasticsearch.requests.$client.${apiEntry.scalaRequest}",
        s"elasticsearch.requests.$client.${apiEntry.scalaResponse}"
      )
      extras ++= extra
      if (managers.contains(client)) {
        managers += (client          -> (managers(client) + "\n\n" + code))
        zioAccessManagers += (client -> (zioAccessManagers(client) + "\n\n" + zioAccessMethods))
        managersImports += (client   -> (managersImports(client) ++ imports))
        implicits += (client         -> (implicits(client) ++ implicitsList))
      } else {
        managers += (client          -> code)
        zioAccessManagers += (client -> zioAccessMethods)
        managersImports += (client   -> imports)
        implicits += (client         -> implicitsList)
      }
    }

    println(s"Generating new files in ${destDir}")
    PathUtils.saveScalaFile(
      (List(s"package ${Constants.namespaceName}.client") ++ extras.map(_._2)).mkString("\n\n"),
      destDir / "enumerations.scala"
    )

    managers.foreach { case (name, code) =>
      val filename = destDir / "managers" / s"${name.capitalize}Manager.scala"
      PathUtils.saveScalaFile(
        elasticsearch.managers.txt
          .Manager(Constants.namespaceName, managersImports.getOrElse(name, Nil).distinct.sorted, name, code)
          .toString(),
        filename
      )
    }

    zioAccessManagers.foreach { case (name, code) =>
      val filename = destDir / "managers" / s"${name.capitalize}Accessors.scala"
      PathUtils.saveScalaFile(
        code.replace("%%SERVICE%%", s"${name.capitalize}Service"),
        filename
      )
    }

    generateClientActions(destDir, apis)
    generateClientActionResolver(destDir, apis)
//
//    fw = new FileWriter(new File(destDir, "ImplicitsForManagers.scala"))
//    fw.write(s"package ${Constants.namespaceName}")
//    fw.write("\n\n")
//    fw.write("import io.circe._\n")
//    fw.write("package object client {")
//    implicits.foreach {
//      case (name, imps) =>
//        imps.foreach { name =>
//          val impName = name(0).toLower + name.substring(1) + "Fmt"
//          fw.write(s"    implicit val $impName = Json.format[$name]\n")
//        }
//    }
//    fw.write("\n\n}\n\n")
//    fw.close()

  }

  def generateClientActions(destDir: File, apis: List[APIEntry]): Unit = {
    val filename = destDir / "client" / "ClientActions.scala"
    var toImport = Set.empty[String]
    requestResponse.foreach { case (req, res) =>
      toImport += req
      toImport += res
    }
    val generated =
      elasticsearch.client.txt
        .ClientActions(Constants.namespaceName, toImport.toList.sorted, requestResponse.toList)
        .toString()
    PathUtils.saveScalaFile(generated, filename)
  }

  def generateRequest(destDir: File, api: APIEntry): Unit = {
    val ddir = api.scope match {
      case "client" =>
        destDir / "requests"
      case default =>
        destDir / "requests" / default
    }
    ddir.createDirectories()

    val filename = ddir / s"${api.scalaRequest}.scala"

    val namespace = api.scope match {
      case "client" =>
        s"${Constants.namespaceName}.requests"
      case default =>
        s"${Constants.namespaceName}.requests.$default"
    }

    var imports = new ListBuffer[String]()

    if (api.scope != "client")
      imports += s"${Constants.namespaceName}.requests.ActionRequest"
    api.extra.foreach { case (k, v) =>
      imports += s"${Constants.namespaceName}.$k"
    }
    val rq        = api.generateRequest.mkString
    val generated = elasticsearch.txt.ScalaRequest(namespace, imports.toSet.toList.sorted, rq).toString()
    PathUtils.saveScalaFile(generated, filename)

  }

  def generateClientActionResolver(destDir: File, apis: List[APIEntry]): Unit = {
    val filename = destDir / "client" / "ClientActionResolver.scala"
    var toImport = Set.empty[String]
    requestResponse.foreach { case (req, res) =>
      toImport += req
      toImport += res
    }
    val generated =
      elasticsearch.client.txt
        .ClientActionResolver(Constants.namespaceName, toImport.toList.sorted, requestResponse.toList)
        .toString()
    PathUtils.saveScalaFile(generated, filename)

  }
}
