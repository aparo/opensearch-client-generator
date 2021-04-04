package io.megl

import better.files._
import io.circe.syntax._
import io.megl.models.Root

object Generator extends App{

  val schemaJson=File.currentWorkingDirectory / "output" / "schema" / "schema.json"

  val schema=for {
    json <- io.circe.parser.parse(schemaJson.contentAsString)
    schema <- json.as[Root]
  } yield schema

  print(schema)


}
