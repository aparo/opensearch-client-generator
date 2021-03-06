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

package io.megl.generators.openapi

import io.circe.Decoder.Result
import io.circe._
final case class Reference($ref: String)

object Reference {
  implicit val referenceCodec: Codec[Reference] = new Codec[Reference] {
    override def apply(c: HCursor): Result[Reference] =
      c.get[String](s"$$ref") match {
        case Left(a)  => Left(a)
        case Right(b) => Right(Reference(b))
      }

    override def apply(a: Reference): Json =
      if (a.$ref.startsWith("#"))
        Json.obj(s"$$ref" -> Json.fromString(a.$ref))
      else
        Json.obj(s"$$ref" -> Json.fromString("#/components/schemas/" + a.$ref))
  }
}
