package sio.macros

import scala.annotation.{ compileTimeOnly, StaticAnnotation }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

import macrocompat._

@compileTimeOnly("typeclass annotation should have been removed by sio.macros but was not")
class optimize() extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro OptimizeMacro.run
}

@bundle
class OptimizeMacro(val c: Context) {
  def run(annottees: c.Expr[Any]*): c.Expr[Any] = {

  }
}