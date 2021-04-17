package io.megl

package object parsers {

  def parseJson()={
    (Constants.specifications / "_json_spec")
      .listRecursively
      .filter(_.name.endsWith(".json"))
      .map{ f =>

      }
  }
}
