package io.megl.common

import scala.annotation.tailrec

object StringUtils {
  /**
   * Converts from camelCase to snake_case
   * e.g.: camelCase => camel_case
   *
   * @param name the camelCase name to convert
   * @return snake_case version of the string passed
   */
  def camelToSnake(name: String): String = {
    @tailrec
    def go(accDone: List[Char], acc: List[Char]): List[Char] = acc match {
      case Nil => accDone
      case a :: b :: c :: tail if a.isUpper && b.isUpper && c.isLower => go(accDone ++ List(a, '_', b, c), tail)
      case a :: b :: tail if a.isLower && b.isUpper => go(accDone ++ List(a, '_', b), tail)
      case a :: tail => go(accDone :+ a, tail)
    }

    go(Nil, name.toList).mkString.toLowerCase
  }
  /*
 * Takes an underscore separated identifier name and returns a camel cased one
 *
 * Example:
 *    underscoreToCamel("this_is_a_1_test") == "thisIsA1Test"
 */

  def underscoreToCamel(name: String):String = "_([a-z\\d])".r.replaceAllIn(name, { m =>
    m.group(1).toUpperCase()
  })

  //  "camelToSnake" should "process right camel case + all upper case + mixed" in {
  //    camelToSnake("COLUMN") shouldBe "column"
  //    camelToSnake("someColumnNameRespectingCamel") shouldBe "some_column_name_respecting_camel"
  //    camelToSnake("columnWITHSomeALLUppercaseWORDS") shouldBe "column_with_some_all_uppercase_words"
  //  }
}
