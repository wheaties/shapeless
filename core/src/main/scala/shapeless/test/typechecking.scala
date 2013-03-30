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

import scala.reflect.macros.{ Context, TypecheckException }

/**
 * A macro that ensures that a code snippet does not typecheck.
 * 
 * Credit: Travis Brown (@travisbrown)
 */
object ShouldNotTypecheck {
  def apply(code: _): Unit = macro applyImpl

  def applyImpl(ctx: Context)(code: ctx.Tree): ctx.Expr[Unit] = {
    try {
      ctx.typeCheck(code)
      ctx.abort(ctx.enclosingPosition, "Type-checking succeeded unexpectedly.\n")
    } catch {
      case e: TypecheckException =>
    }
    ctx.universe.reify(())
  }
}
