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

package shapeless

import scala.language.experimental.macros

import scala.reflect.macros.{ Context, Macro }

object SingletonTypes {
  type ^[T](t: T) = macro SingletonTypeMacros.singletonType[T]

  case class Box[T](value: T)

  implicit def witnessBox[T]: Box[T] = macro SingletonTypeMacros.witnessBox[T]

  def singleton[T](implicit box: Box[T]): T = box.value
}

trait SingletonTypeMacros extends Macro {
  import c.universe._

  def eval[T](t: c.Tree) = c.eval(c.Expr[T](c.resetAllAttrs(t.duplicate)))

  def singletonType[T](t: c.Expr[T]) =
    TypeTree(ConstantType(Constant(
      eval[T](t.tree)
    )))

  def witnessBox[T: c.WeakTypeTag] = {
    weakTypeOf[T] match {
      case t @ ConstantType(Constant(s)) =>
        val T = TypeTree(t)
        val l = Literal(Constant(s))
        c.Expr[T](q"new Box[$T]($l)")
      case _ => c.abort(c.enclosingPosition, "Type argument must be a singleton type")
    }
  }
}

class SNat(val value: Int) {
  type N
  override def toString = s"SNat($value) { type N = Int($value) }"
}

object SNat {
  implicit def apply(i: Int) = macro SNatMacros.toSNat

  implicit def natToInt(snat: SNat): Int = snat.value
}

trait SNatMacros extends Macro with SingletonTypeMacros {
  import c.universe._

  def toSNat(i: c.Expr[Int]): c.Expr[SNat] = {
    val n = eval[Int](i.tree)
    if (n < 0)
      c.abort(c.enclosingPosition, s"A SNat cannot represent $n")
    val N0 = TypeTree(ConstantType(Constant(n)))
    c.Expr(q"new SNat($n) { type N = $N0 }")
  }
}

trait SNatOpMacros extends Macro {
  import c.universe._

  class WitnessAux[E] {
    def apply[R](op: (Int, Int) => R, validate: R => Boolean = (_: R) => true)(implicit foo : c.WeakTypeTag[E]): c.Expr[E] = {
      val expr = weakTypeOf[E]
      val res: c.Type = expr.normalize match {
        case TypeRef(_, _, List(ConstantType(Constant(a: Int)), ConstantType(Constant(b: Int)))) =>
          val r = op(a, b)
          if (!validate(r))
            c.abort(c.enclosingPosition, s"The result of a SNatOp cannot represent $r")
          ConstantType(Constant(r))
      }
      c.Expr(q"new $expr { type Result = $res }")
    }
  }
  
  def witness[E] = new WitnessAux[E]
  
  def plus[A : c.WeakTypeTag, B : c.WeakTypeTag] = witness[A ^+ B](_ + _)
  def minus[A : c.WeakTypeTag, B : c.WeakTypeTag] = witness[A ^- B](_ - _, (_: Int) >= 0)
  def times[A : c.WeakTypeTag, B : c.WeakTypeTag] = witness[A ^* B](_ * _)
  def div[A : c.WeakTypeTag, B : c.WeakTypeTag] = witness[A ^/ B](_ / _)
  def mod[A : c.WeakTypeTag, B : c.WeakTypeTag] = witness[A ^% B](_ % _)

  def lt[A : c.WeakTypeTag, B : c.WeakTypeTag] = witness[A ^< B](_ < _)
  def lteq[A : c.WeakTypeTag, B : c.WeakTypeTag] = witness[A ^<= B](_ <= _)
  def gt[A : c.WeakTypeTag, B : c.WeakTypeTag] = witness[A ^> B](_ > _)
  def gteq[A : c.WeakTypeTag, B : c.WeakTypeTag] = witness[A ^>= B](_ >= _)
  def max[A : c.WeakTypeTag, B : c.WeakTypeTag] = witness[A Max B](_ max _)
  def min[A : c.WeakTypeTag, B : c.WeakTypeTag] = witness[A Min B](_ min _)
}

trait SNatExpr {
  type Result
}

trait ^+[A, B] extends SNatExpr

object ^+ {
  implicit def apply[A, B] = macro SNatOpMacros.plus[A, B]

  def apply(a: SNat, b: SNat)(implicit plus: a.N ^+ b.N): (a.N ^+ b.N) { type Result = plus.Result } = plus
}

trait ^-[A, B] extends SNatExpr

object ^- {
  implicit def apply[A, B] = macro SNatOpMacros.minus[A, B]

  def apply(a: SNat, b: SNat)(implicit minus: a.N ^- b.N): (a.N ^- b.N) { type Result = minus.Result } = minus
}

trait ^*[A, B] extends SNatExpr

object ^* {
  implicit def apply[A, B] = macro SNatOpMacros.times[A, B]

  def apply(a: SNat, b: SNat)(implicit times: a.N ^* b.N): (a.N ^* b.N) { type Result = times.Result } = times
}

trait ^/[A, B] extends SNatExpr

object ^/ {
  implicit def apply[A, B] = macro SNatOpMacros.div[A, B]

  def apply(a: SNat, b: SNat)(implicit div: a.N ^/ b.N): (a.N ^/ b.N) { type Result = div.Result } = div
}

trait ^%[A, B] extends SNatExpr

object ^% {
  implicit def apply[A, B] = macro SNatOpMacros.mod[A, B]

  def apply(a: SNat, b: SNat)(implicit mod: a.N ^% b.N): (a.N ^% b.N) { type Result = mod.Result } = mod
}

trait ^<[A, B] extends SNatExpr

object ^< {
  implicit def apply[A, B] = macro SNatOpMacros.lt[A, B]

  def apply(a: SNat, b: SNat)(implicit lt: a.N ^< b.N): (a.N ^< b.N) { type Result = lt.Result } = lt
}

trait ^<=[A, B] extends SNatExpr

object ^<= {
  implicit def apply[A, B] = macro SNatOpMacros.lteq[A, B]

  def apply(a: SNat, b: SNat)(implicit lteq: a.N ^<= b.N): (a.N ^<= b.N) { type Result = lteq.Result } = lteq
}

trait ^>[A, B] extends SNatExpr

object ^> {
  implicit def apply[A, B] = macro SNatOpMacros.gt[A, B]

  def apply(a: SNat, b: SNat)(implicit gt: a.N ^> b.N): (a.N ^> b.N) { type Result = gt.Result } = gt
}

trait ^>=[A, B] extends SNatExpr

object ^>= {
  implicit def apply[A, B] = macro SNatOpMacros.gteq[A, B]

  def apply(a: SNat, b: SNat)(implicit gteq: a.N ^>= b.N): (a.N ^>= b.N) { type Result = gteq.Result } = gteq
}

trait Max[A, B] extends SNatExpr

object Max {
  implicit def apply[A, B] = macro SNatOpMacros.max[A, B]

  def apply(a: SNat, b: SNat)(implicit max: a.N Max b.N): (a.N Max b.N) { type Result = max.Result } = max
}

trait Min[A, B] extends SNatExpr

object Min {
  implicit def apply[A, B] = macro SNatOpMacros.min[A, B]

  def apply(a: SNat, b: SNat)(implicit min: a.N Min b.N): (a.N Min b.N) { type Result = min.Result } = min
}

trait ^==[E, R]

object ^== {
  implicit def apply[E <: SNatExpr](implicit expr: E): E ^== expr.Result = new (E ^== expr.Result) {}
}
