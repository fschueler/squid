// Copyright 2017 EPFL DATA Lab (data.epfl.ch)
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package squid
package ir

import squid.utils.meta.RuntimeUniverseHelpers
import utils._

trait Lowering extends Transformer {
  val base: ir.AST with ClassEmbedder
  import base._
  
  val warnRecursiveEmbedding = true
  
  val loweredPhases: Set[Symbol]
  
  import ClassEmbedder.Error._
  def transform(rep: Rep): Rep = rep match {
    case RepDef(ma @ MethodApp(self, sym, targs, argss, retTyp)) if ma.phase exists loweredPhases =>
      val fullArgss = if (sym.owner.isModuleClass) argss
        else Args(self)::argss
      val fullTargs = self.typ.tpe.typeArgs.map(new TypeRep(_)) ++ targs // FIXME should use `baseType`! the type of `self` could be a subtype with different type arguments...
      //println(s"Getting $sym in ${self.typ.tpe} fullTargs $fullTargs")
      methodDef(sym, fullTargs) match {
        case Right(Code(r)) =>
          //println(s"Lowering $ma with ${r|>showRep}")
          val res = fullArgss.foldLeft(r) {
            case (r, Args(reps @ _*)) =>
              val ruh = RuntimeUniverseHelpers
              val typ = r.typ.typeArgs.last // TODO be careful that a method implemented without parameter list can implement a method with an empty list (eg toString), which can make this crash
              base.rep(MethodApp(r, ruh.FunctionType.symbol(reps.size).toType member ruh.sru.TermName("apply") asMethod, Nil, Args(reps:_*)::Nil, typ))
            case (r, ArgsVarargs(Args(reps @ _*), Args(vreps @ _*))) =>
              val ruh = RuntimeUniverseHelpers
              val functionArity = reps.size + vreps.size
              val typ = r.typ.typeArgs.last
              val varargsAsSeq = base.rep(MethodApp(base.rep(StaticModule("scala.collection.Seq")), ruh.FunctionType.symbol(vreps.size).toType member ruh.sru.TermName("apply") asMethod, Nil, Args(vreps:_*)::Nil, ruh.SequenceType.apply(typ)))
              base.rep(MethodApp(r, ruh.FunctionType.symbol(functionArity).toType member ruh.sru.TermName("apply") asMethod, Nil, Args((reps :+ varargsAsSeq):_* )::Nil, typ))
          }
          ascribe(res, retTyp) // We ascribe so that if the body is, e.g., `???`, we don't end up with ill-typed code. 
        case Left(Recursive) =>
          if (warnRecursiveEmbedding) System.err.println(s"Warning: Recursive value ${sym fullName} cannot be fully embedded.")
          rep
        case Left(Missing) =>
          System.err.println(s"Warning: Could not find definition for lowered method: ${sym fullName}${sym typeSignature} @ phase ${ma.phase get}") // TODO B/W
          rep
        case _ => spuriousWarning
      }
    case _ => rep
  }
  
}
trait OnlineDesugaring extends OnlineOptimizer with Lowering with ClassEmbedder { self: ir.AST with ClassEmbedder =>
  val loweredPhases = Set('Sugar)
}
