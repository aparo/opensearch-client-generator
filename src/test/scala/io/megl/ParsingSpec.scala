package io.megl

import zio._
import zio.console._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._


object ParsingSpec extends DefaultRunnableSpec {
  def spec = suite("ParsingSpec")(
    testM("Correctly parse all the schema.json") {
      for {
        _      <- sayHello
        output <- TestConsole.output
      } yield assert(output)(equalTo(Vector("Hello, World!\n")))
    }
  )
}
