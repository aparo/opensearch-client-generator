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

package io.megl.generators

import io.megl.common.Algorithms
import io.megl.parsers.TSFileMetadata
import io.megl.parsers.jsonspec.APIEntry

import _root_.scala.collection.mutable.ListBuffer

object GeneratorContext {
  lazy val apiEntries: List[APIEntry] = io.megl.parsers.parseJson()
  lazy val entities: Map[String, TSFileMetadata] = io.megl.parsers.parseEntities().map(e => e.name -> e).toMap

  val traitsForScala = Set(
//    "CatBase",
    "ShardsOperationResponseBase",
    "DynamicResponseBase",
//    "ErrorResponseBase",
    "DictionaryResponseBase",
    "IndicesResponseBase",
    "ResponseBase",
    "AcknowledgedResponseBase",
    "RequestBase",
    "NodesResponseBase",
    "AnalyzerBase",
    "TokenizerBase",
    "CharFilterBase",
    "TokenFilterBase",
    "CompoundWordTokenFilterBase",
    "ProcessorBase",
//    "BulkResponseItemBase",
    "WriteResponseBase",
    "CustomResponseBuilderBase",
    "PipelineAggregationBase",
    "BucketAggregationBase")


  lazy val dependencyTree: List[String] = {
    val seq = new ListBuffer[(String, String)]()
    entities.foreach {
      e =>
        e._2.depends.foreach(d => seq += (e._1 -> d))
    }
    val deps = Algorithms.tsort(seq).toList.reverse
    val diff = entities.keySet.diff(deps.toSet)
    diff.toList ++ deps
  }
  //  println(tsort(Seq((1, 2), (2, 4), (3, 4), (3, 2), (1,3))))
  //  println("sorting")
  //  println(deppendencyTree)

  /***
   * Fix code to manage scala entities
   */
  def fixContextForScala(): Unit ={
    GeneratorContext.traitsForScala.foreach{
      t =>
        entities(t).isTrait=true
    }
  }

}
