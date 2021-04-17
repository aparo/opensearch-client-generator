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

package io.megl

import better.files.File

object Constants {

  val destPackage: String   = "opensearch"
  val namespaceName: String = "opensearch"
  val esClassName: String   = "OpenSearch"

  lazy val specifications: File     = File.currentWorkingDirectory / "specification" / "specs"

  lazy val devESSourcePath: File     = File.home / "Project" / "OpenSearch"
  lazy val devRestAPIPath: File      = File.home / "Project" / "opensearch-client-generator"
  lazy val devScalaAPIDestPath: File = File.home / "Project" / "opensearch-scala-client"

}
