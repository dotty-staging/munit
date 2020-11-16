package munit.internal

import munit.Clue
import munit.Location
import scala.quoted._

object MacroCompat {

  trait LocationMacro {
    inline implicit def generate: Location = ${ locationImpl() }
  }

  def locationImpl()(using QuoteContext): Expr[Location] = {
    import qctx.reflect._
    val pos = Position.ofMacroExpansion
    val path = pos.sourceFile.jpath.toString
    val startLine = pos.startLine + 1
    '{ new Location(${Expr(path)}, ${Expr(startLine)}) }
  }

  trait ClueMacro {
    inline implicit def generate[T](value: T): Clue[T] = ${ clueImpl('value) }
  }

  def clueImpl[T:Type](value: Expr[T])(using QuoteContext): Expr[Clue[T]] = {
    import qctx.reflect._
    val source = Term.of(value).pos.sourceCode
    val valueType = Type.show[T]
    '{ new Clue(${Expr(source)}, $value, ${Expr(valueType)}) }
  }

}
