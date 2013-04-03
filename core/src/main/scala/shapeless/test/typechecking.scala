/*
 * Copyright (c) 2013 Miles Sabin 
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

package shapeless.test

import scala.language.experimental.macros

import java.util.regex.Pattern

import scala.reflect.macros.{ Context, TypecheckException }

/**
 * A utility which ensures that a code fragment does not typecheck.
 * 
 * Credit: Travis Brown (@travisbrown)
 */
object illTyped {
  def apply(codePat: _*): Unit = macro applyImpl

  def applyImpl(c: Context)(codePat: c.Tree*): c.Expr[Unit] = {
    import c.universe._
    
    val (code, expPat, expMsg) = codePat match {
      case code :: Nil => (code, null, "Expected some error.")
      case code :: Literal(Constant(s: String)) :: Nil =>
        (code, Pattern.compile(s, Pattern.CASE_INSENSITIVE), "Expected error matching: "+s)
    }
    
    try {
      c.typeCheck(code)
      c.abort(c.enclosingPosition, "Type-checking succeeded unexpectedly.\n"+expMsg)
    } catch {
      case e: TypecheckException =>
        val msg = e.getMessage
        if((expPat ne null) && !(expPat.matcher(msg)).matches)
          c.abort(c.enclosingPosition, "Type-checking failed in an unexpected way.\n"+expMsg+"\nActual error: "+msg)
    }
    
    reify(())
  }
}