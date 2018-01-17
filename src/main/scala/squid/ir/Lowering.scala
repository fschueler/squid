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

import squid.utils.meta.{RuntimeUniverseHelpers => ruh}
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
              val typ = r.typ.typeArgs.last // TODO be careful that a method implemented without parameter list can implement a method with an empty list (eg toString), which can make this crash
              base.rep(MethodApp(r, ruh.FunctionType.symbol(reps.size).toType member ruh.sru.TermName("apply") asMethod, Nil, Args(reps:_*)::Nil, typ))
              
            case (r, ArgsVarargs(Args(reps @ _*), Args(vreps @ _*))) =>
              
              val functionArity = reps.size + 1 // the varargs are wrapped in a Seq, which is only one argument
              
              // It's a little tricky to know what the type of the formal arguments to the method should be, based on the 
              // type arguments given to the method invocation; thankfully Seq is covariant, so since we can assume the 
              // code is well-typed, it is sufficient to make a Seq of the least upper bound type of the arguments.
              val typ = new TypeRep(ruh.sru.lub(vreps.iterator.map(_.typ.tpe).toList))
              
              // transform repeated args into a Seq.apply[typ](vargs)
              val varargsAsSeq = methodApp(
                staticModule("scala.collection.Seq"),
                loadMtdSymbol(
                  loadTypSymbol("scala.collection.generic.GenericCompanion"),
                  "apply",
                  None),
                typ :: Nil,
                Args()(vreps: _*) :: Nil,
                staticTypeApp(
                  loadTypSymbol("scala.collection.Seq"),
                  typ :: Nil)
              )
              
              base.rep(MethodApp(r, ruh.FunctionType.symbol(functionArity).toType member ruh.sru.TermName("apply") asMethod, Nil, Args((reps :+ varargsAsSeq):_* )::Nil, typ))
              
            case (r, avs@ArgsVarargSpliced(Args(reps @ _*), vreps)) =>
              val typ = r.typ.typeArgs.last
              val functionArity = reps.size + 1 // number arguments plus one Seq argument
              base.rep(MethodApp(r, ruh.FunctionType.symbol(functionArity).toType member ruh.sru.TermName("apply") asMethod, Nil, Args((reps :+ vreps) :_*)::Nil, typ))
              
          }
          ascribe(res, retTyp) // We ascribe so that if the body is, e.g., `???`, we don't end up with ill-typed code. 
        case Left(Recursive) =>
          if (warnRecursiveEmbedding) System.err.println(s"Warning: Recursive value ${sym fullName} cannot be fully lowered/inlined.")
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
