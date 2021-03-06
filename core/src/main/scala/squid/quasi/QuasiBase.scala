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
package quasi

import squid.ir.RewriteAbort
import squid.ir.Transformer
import utils._
import utils.typing._
import squid.lang.Base
import squid.lang.InspectableBase
import squid.lang.IntermediateBase
import utils.MacroUtils.MacroSetting

import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable


/** Provides the macro infrastructure to allow for using quasiquotes ir"foo" and quasicode ir{foo}, 
  * which are more type-safe interfaces to manipulate code than the Base functions. */
/* TODO implement feature restrictions in quasiquotes, so pattern QQs cannot be defined for non-inspectable bases...
 * Also make Liftable instances nicer since we are now be able to assume QQ/QC can be used on all bases */
trait QuasiBase {
self: Base =>
  
  
  // TODO move these to IntermediateBase
  
  /** Should substitute holes for the given definitions */
  // TODO impl correctly
  //def substitute(r: Rep, defs: Map[String, Rep]): Rep = substituteLazy(r, defs map (kv => kv._1 -> (() => kv._2)))
  //def substituteLazy(r: Rep, defs: Map[String, () => Rep]): Rep
  def substitute(r: => Rep, defs: Map[String, Rep]): Rep
  def substituteLazy(r: => Rep, defs: Map[String, () => Rep]): Rep = substitute(r, defs map (kv => kv._1 -> kv._2()))
  
  /** Not yet used: should eventually be defined by all IRs as something different than hole */
  def freeVar(name: String, typ: TypeRep) = hole(name, typ)
  
  /** Traditional hole with CMTT semantics, in general position; this should extract a `Rep` */
  def hole(name: String, typ: TypeRep): Rep
  
  /** Traditional hole with CMTT semantics, in spliced position (e.g., `$xs` in `ir"List($xs*)"`); this should extract a `Seq[Rep]` */
  def splicedHole(name: String, typ: TypeRep): Rep
  
  /** Higher-Order Pattern (Variable) hole; 
    * this should check that the extracted term does not contain any reference to a bound value contained in the `no` 
    * parameter, and it should extract a function term with the arity of the `yes` parameter. */
  def hopHole(name: String, typ: TypeRep, yes: List[List[BoundVal]], no: List[BoundVal]): Rep
  
  /** Pattern hole in type position */
  def typeHole(name: String): TypeRep
  
  
  
  
  
  
  /* --- --- --- Provided defs --- --- --- */
  
  
  final def substitute(r: => Rep, defs: (String, Rep)*): Rep =
  /* if (defs isEmpty) r else */  // <- This "optimization" is not welcome, as some IRs (ANF) may relie on `substitute` being called for all insertions
    substitute(r, defs.toMap)
  
  protected def mkCode[T,C](r: Rep): Code[T,C] = new Code[T,C] {
    val rep = r
    override def equals(that: Any): Boolean = that match {
      case that: Code[_,_] => rep =~= that.rep
      case _ => false
    }
  }
  
  abstract class AnyCode[+T] protected() extends TypeErased with ContextErased {
    type Typ <: T
    val rep: Rep
    
    def unsafe_asClosedCode: ClosedCode[T] = `internal Code`(rep)
    
    // In the future: have a mehtod that actually checks contexts at runtime, once we have reified hygienic contexts...
    //def asIR[C:Ctx]: Option[IR[T,C]] = ???
    
    def =~= (that: AnyCode[_]): Boolean = rep =~= that.rep
    
    def transformWith(trans: ir.Transformer{val base: self.type}) = AnyCode[T](trans.pipeline(rep))
    
    /** Useful when we have non-denotable types (e.g., extracted types) */
    def withTypeOf[Typ >: T](x: AnyCode[Typ]) = this: AnyCode[Typ]
    
  }
  object AnyCode {
    def apply[T](rep: Rep): AnyCode[T] = mkCode(rep)
    def unapply[T](c: AnyCode[T]): Option[Rep] = Some(c.rep)
  }
  
  type OpenCode[+T] = Code[T,Bottom]
  type ClosedCode[+T] = Code[T,Any]
  
  object Code {
    def apply[T,C](rep: Rep): Code[T,C] = mkCode(rep)
    def unapply[T,C](ir: Code[T,C]): Option[Rep] = Some(ir.rep)
  }
  
  // Unsealed to allow for abstract constructs to inherit from IR, for convenience.
  abstract class Code[+T, -C] protected() extends AnyCode[T] {
    
    // Note: cannot put this in `IntermediateIROps`, because of the path-dependent type
    def Typ(implicit ev: self.type <:< (self.type with IntermediateBase)): CodeType[Typ] = {
      // Ninja path-dependent typing (relies on squid.utils.typing)
      val s: self.type with IntermediateBase = self
      val r: s.Rep = substBounded[Base,self.type,s.type,({type λ[X<:Base] = X#Rep})#λ](rep)
      s.`internal CodeType`(s.repType(r))
    }
    
    // type Ctx >: C
    /* ^ this is the 'type-correct' definition: given an x:IR[T,C], we cannot really conclude that the "real" context of
     * x is C, since because of contravariance it *could be* any type less specific than C, such as Any. */
    type Ctx = C @uncheckedVariance
    /* ^ however this definition is much more useful, because we sometimes want to refer to a term's "observed" context 
     * even when it may not be the "real", most specific context.
     * This is safe, because Ctx is a phantom type and so no values will be misinterpreted because of this variance 
     * violation. 
     * An alternative (more cumbersome) solution that does not require @uncheckedVariance is to use type inference, as in:
     *   `def ctxRef[C](x:IR[Any,C]) = new{ type Ctx = C }; val c = ctxRef(x); x : IR[T,c.Ctx]` */ 
    
    val rep: Rep
    
    //// dangerous... -> when Rep extends IR, this makes SchedulingANF consider `x` and `(x:t)` equal, for example, 
    //// which has weird consequences (eg scheduling one as the other):
    //override def equals(that: Any): Boolean = that match {
    //  case that: IR[_,_] => rep =~= that.rep
    //  case _ => false
    //}
    
    def cast[S >: T]: Code[S, C] = this
    def erase: Code[Any, C] = this
    
    override def transformWith(trans: ir.Transformer{val base: self.type}) = Code[T,C](trans.pipeline(rep))
    
    /** Useful when we have non-denotable types (e.g., extracted types) 
      * -- in fact, now that we have `Ctx = C @uncheckedVariance` it's not really useful anymore... */
    override def withTypeOf[Typ >: T](x: AnyCode[Typ]) = this: Code[Typ,C]
    
    override def unsafe_asClosedCode = this.asInstanceOf[ClosedCode[T]]
    
    /** Useful when we have non-denotable contexts (e.g., rewrite rule contexts) */
    def withContextOf[Ctx <: C](x: Code[Any, Ctx]) = this: Code[T,Ctx]
    //def unsafeWithContextOf[Ctx](x: IR[Any, Ctx]) = this.asInstanceOf[IR[T,Ctx]]  // does not seem to be too useful...
    
    override def toString: String = {
      val repStr = showRep(rep)
      val quote = if ((repStr contains '\n') || (repStr contains '"')) "\"" * 3 else "\""
      s"code$quote$repStr$quote"
    }
  }
  def `internal Code`[Typ,Ctx](rep: Rep) = Code[Typ,Ctx](rep) // mainly for macros
  type SomeCode = AnyCode[_] // shortcut; avoids showing messy existentials
  
  
  sealed case class CodeType[T] private[quasi](rep: TypeRep) extends TypeErased {
    type Typ = T
    def <:< (that: CodeType[_]) = rep <:< that.rep
    def =:= (that: CodeType[_]) = rep =:= that.rep
    def nullValue(implicit ev: self.type <:< (self.type with IntermediateBase)) = Predef.nullValue(this,ev)
  }
  def `internal CodeType`[Typ](trep: TypeRep) = CodeType[Typ](trep) // mainly for macros
  
  trait TypeErased { type Typ }
  trait ContextErased { type Ctx }
  
  /** Allows things like TypeOf[ir.type] and TypeOf[irTyp.type], useful when extracted values have non-denotable types  */
  type TypeOf[QT <: TypeErased] = QT#Typ
  
  /** Allows things like ContextOf[ir.type], useful when extracted values have non-denotable contexts  */
  type ContextOf[QT <: ContextErased] = QT#Ctx
  
  
  def codeTypeOf[T: CodeType] = implicitly[CodeType[T]]
  def typeRepOf[T: CodeType] = implicitly[CodeType[T]].rep
  
  
  
  val Predef  = new Predef[DefaultQuasiConfig]
  
  class Predef[QC <: QuasiConfig] {
    val base: self.type = self // macro expansions will make reference to it
    
    type AnyCode[+T] = self.AnyCode[T]
    type Code[+T,-C] = self.Code[T,C]
    type CodeType[T] = self.CodeType[T]
    
    type OpenCode[+T] = self.OpenCode[T]
    type ClosedCode[+T] = self.ClosedCode[T]
    
    type __* = self.__*
    
    val Const: self.Const.type = self.Const
    def codeTypeOf[T: CodeType] = self.codeTypeOf[T]
    def typeRepOf[T: CodeType] = self.typeRepOf[T]
    
    def nullValue[T: CodeType](implicit ev: base.type <:< (base.type with IntermediateBase)): Code[T,{}] = {
      val b: base.type with IntermediateBase = ev(base)
      //b.nullValue[T](irTypeOf[T]) // should work, but Scala doesn't like it
      b.nullValue[T](codeTypeOf[T].asInstanceOf[b.CodeType[T]])
    }
    // Unsafe version:
    //def nullValue[T: CodeType]: IR[T,{}] = {
    //  val b = base.asInstanceOf[base.type with IntermediateBase]
    //  b.nullValue[T](irTypeOf[T].asInstanceOf[b.CodeType[T]]) }
    
    @compileTimeOnly("Abort(msg) should only be called from the body of a rewrite rule. " +
      "If you want to abort a rewriting from a function called in the rewrite rule body, " +
      "use `throw RewriteAbort(msg)` instead.")
    def Abort(msg: String = ""): Nothing = throw new IllegalAccessError("Abort was called outside a rewrite rule!")
    
    object Return {
      @compileTimeOnly("Return cannot be called outside of a rewrite rule.")
      def apply[T,C](x: Code[T,C]): Code[T,C] = x
      
      @compileTimeOnly("Return.transforming cannot be called outside of a rewrite rule.")
      def transforming[A,CA,T,C](a: Code[A,CA])(f: Code[A,CA] => Code[T,C]): Code[T,C] = f(a)
      
      @compileTimeOnly("Return.transforming cannot be called outside of a rewrite rule.")
      def transforming[A,CA,B,CB,T,C](a: Code[A,CA], b: Code[B,CB])(f: (Code[A,CA], Code[B,CB]) => Code[T,C]): Code[T,C] = f(a,b)
      
      @compileTimeOnly("Return.transforming cannot be called outside of a rewrite rule.")
      def transforming[A,CA,B,CB,D,CD,T,C](a: Code[A,CA], b: Code[B,CB], d: Code[D,CD])(f: (Code[A,CA], Code[B,CB], Code[D,CD]) => Code[T,C]): Code[T,C] = f(a,b,d)
      
      @compileTimeOnly("Return.transforming cannot be called outside of a rewrite rule.")
      def transforming[A,CA,T,C](as: List[Code[A,CA]])(f: List[Code[A,CA]] => Code[T,C]): Code[T,C] = f(as)
      
      @compileTimeOnly("Return.recursing cannot be called outside of a rewrite rule.")
      def recursing[T,C](cont: Transformer{val base: self.type} => Code[T,C]): Code[T,C] = {
        cont(new Transformer { // Dummy transformer that doesn't do anything
          override def transform(rep: base.Rep) = rep
          override val base: self.type with InspectableBase = self.asInstanceOf[self.type with InspectableBase]
        })
      }
    }
    
    import scala.language.dynamics
    object ? extends Dynamic {
      @compileTimeOnly("The `?` syntax can only be used inside quasiquotes and quasicode.")
      def selectDynamic(name: String): Nothing = ???
    }
    
    import scala.language.experimental.macros
    
    implicit class QuasiContext(private val ctx: StringContext) extends self.Quasiquotes.QuasiContext(ctx)
    
    implicit def implicitType[T]: CodeType[T] = macro QuasiBlackboxMacros.implicitTypeImpl[QC, T]
    
    object dbg {
      /** import this `implicitType` explicitly to shadow the non-debug one */ 
      @MacroSetting(debug = true) implicit def implicitType[T]: CodeType[T] = macro QuasiBlackboxMacros.implicitTypeImpl[QC, T]
    }
    
    implicit def unliftFun[A,B:CodeType,C](f: Code[A => B,C]): Code[A,C] => Code[B,C] = a => Code(tryInline(f.rep,a.rep)(typeRepOf[B]))
  }
  
  
  /** For easier migration from code bases that used the old `IR`-named interfaces */
  val LegacyPredef = new LegacyPredef{}
  
  trait LegacyPredef {
    
    type IR[+T,-C] = Code[T,C]
    type IRType[T] = CodeType[T]
    val IRType = CodeType
    def irTypeOf[T: CodeType] = implicitly[CodeType[T]]
    
    /** This is to remove warnings due to use of `ir` interpolators instead of `code`;
      * Note that if both this and the original one from SuppressWarning, the implicit will be ambiguous and the warning
      * will still be reported... */
    implicit def `use of ir instead of code` = SuppressWarning.`use of ir instead of code`
    
  }
  
  
  def `internal abort`(msg: String): Nothing = throw RewriteAbort(msg)
  
  
  
  /** Artifact of a term extraction: map from hole name to terms, types and spliced term lists */
  type Extract = (Map[String, Rep], Map[String, TypeRep], Map[String, Seq[Rep]])
  type Extract_? = Extract |> Option
  protected final val EmptyExtract: Extract = (Map(), Map(), Map())
  protected final val SomeEmptyExtract: Option[Extract] = EmptyExtract |> some
  @inline protected final def mkExtract(rs: String -> Rep *)(ts: String -> TypeRep *)(srs: String -> Seq[Rep] *): Extract = (rs toMap, ts toMap, srs toMap)
  @inline protected final def repExtract(rs: String -> Rep *): Extract = mkExtract(rs: _*)()()
  protected def extractedKeys(e: Extract): Set[String] = e._1.keySet++e._2.keySet++e._3.keySet
  
  protected def mergeOpt(a: Option[Extract], b: => Option[Extract]): Option[Extract] = for { a <- a; b <- b; m <- merge(a,b) } yield m
  protected def merge(a: Extract, b: Extract): Option[Extract] = {
    b._1 foreach { case (name, vb) => (a._1 get name) foreach { va => if (!mergeableReps(va, vb)) return None } }
    b._3 foreach { case (name, vb) => (a._3 get name) foreach { va =>
      if (va.size != vb.size || !(va.iterator zip vb.iterator forall {case (vva,vvb) => mergeableReps(vva, vvb)})) return None } }
    val typs = a._2 ++ b._2.map {
      case (name, t) =>
        a._2 get name map (t2 => name -> mergeTypes(t, t2).getOrElse(return None)) getOrElse (name -> t)
    }
    val splicedVals = a._3 ++ b._3
    val vals = a._1 ++ b._1
    Some(vals, typs, splicedVals)
  }
  protected def mergeAll(as: TraversableOnce[Option[Extract]]): Option[Extract] = {
    if (as isEmpty) return Some(EmptyExtract)
    
    //as.reduce[Option[Extract]] { case (acc, a) => for (acc <- acc; a <- a; m <- merge(acc, a)) yield m }
    /* ^ not good as it evaluates all elements of `as` (even if it's an Iterator or Stream) */
    
    val ite = as.toIterator
    var res = ite.next()
    while(ite.hasNext && res.isDefined) res = mergeOpt(res, ite.next())
    res
  }
  
  def mergeableReps(a: Rep, b: Rep): Boolean = a =~= b
  
  def mergeTypes(a: TypeRep, b: TypeRep): Option[TypeRep] =
    // Note: We can probably do better than =:=, but that is up to the implementation of TypingBase to override it
    if (typEq(a,b)) Some(a) else None
  
  
  private var extracting = false
  protected def isExtracting = extracting
  final protected def isConstructing = !isExtracting
  def wrapConstruct(r: => Rep) = { val old = extracting; extracting = false; try r finally { extracting = old } }
  def wrapExtract  (r: => Rep) = { val old = extracting; extracting = true;  try r finally { extracting = old } }
  //def wrapExtract  (r: => Rep) = { val old = extracting; extracting = true; println("BEG EXTR"); try r finally { println("END EXTR"); extracting = old } }
  // ^ TODO find a solution cf by-name params, which creates owner corruption in macros

  
  type __* = Seq[Code[_,_]] // used in insertion holes, as in:  ${xs: __*}
  
  
  
  /** TODO make it more generic: use Liftable! */
  /* To support insertion syntax `$xs` (in ction) or `$$xs` (in xtion) */
  @compileTimeOnly("Unquote syntax ${...} cannot be used outside of a quasiquote.")
  def $[T,C](q: Code[T,C]*): T = ??? // TODO B/E  -- also, rename to 'unquote'?
  //def $[T,C](q: IR[T,C]): T = ??? // Actually unnecessary
  @compileTimeOnly("Unquote syntax ${...} cannot be used outside of a quasiquote.")
  def $[A,B,C](q: Code[A,C] => Code[B,C]): A => B = ??? // TODO rely on implicit liftFun instead?
  
  def $Code[T](q: AnyCode[T]): T = ??? // TODO B/E  -- also, rename to 'unquote'?
  def $Code[A,B](q: AnyCode[A] => AnyCode[B]): A => B = ???
  
  /* To support hole syntax `xs?` (old syntax `$$xs`) (in ction) or `$xs` (in xtion)  */
  @compileTimeOnly("Unquote syntax ${...} cannot be used outside of a quasiquote.")
  def $$[T](name: Symbol): T = ???
  /* To support hole syntax `$$xs: _*` (in ction) or `$xs: _*` (in xtion)  */
  @compileTimeOnly("Unquote syntax ${...} cannot be used outside of a quasiquote.")
  def $$_*[T](name: Symbol): Seq[T] = ???
  
  
  implicit def liftFun[A:CodeType,B,C](qf: Code[A,C] => Code[B,C]): Code[A => B,C] = {
    val bv = bindVal("lifted", typeRepOf[A], Nil) // add TODO annotation recording the lifting?
    val body = qf(Code(bv |> readVal)).rep
    Code(lambda(bv::Nil, body))
  }
  implicit def liftCodeFun[A:CodeType,B](qf: AnyCode[A] => AnyCode[B]): AnyCode[A => B] = {
    val bv = bindVal("lifted", typeRepOf[A], Nil) // add TODO annotation recording the lifting?
    val body = qf(AnyCode(bv |> readVal)).rep
    AnyCode(lambda(bv::Nil, body))
  }
  implicit def unliftFun[A,B:CodeType,C](qf: Code[A => B,C]): Code[A,C] => Code[B,C] = x => {
    Code(app(qf.rep, x.rep)(typeRepOf[B]))
  }
  implicit def unliftCodeFun[A,B:CodeType](qf: AnyCode[A => B]): AnyCode[A] => AnyCode[B] = x => {
    AnyCode(app(qf.rep, x.rep)(typeRepOf[B]))
  }
  
  import scala.language.experimental.macros
  
  class Quasiquotes[QC <: QuasiConfig] {
    val base: QuasiBase.this.type = QuasiBase.this
    
    implicit class QuasiContext(private val ctx: StringContext) {
      
      // TODO deprecate in favor or IR
      object ir { // TODO try to refine the types..?
        //def apply(inserted: Any*): Any = macro QuasiMacros.applyImpl[QC]
        //def unapply(scrutinee: Any): Any = macro QuasiMacros.unapplyImpl[QC]
        // ^ versions above give less hints to macro-blind IDEs, but may be faster to compile 
        // as they involve an easier (trivial) subtype check: macro_result.tpe <: Any (performed by scalac)
        def apply(inserted: Any*): SomeCode = macro QuasiMacros.applyImpl[QC]
        def unapply(scrutinee: SomeCode): Any = macro QuasiMacros.unapplyImpl[QC]
      }
      
      object dbg_ir { // TODO try to refine the types..?
        @MacroSetting(debug = true) def apply(inserted: Any*): Any = macro QuasiMacros.applyImpl[QC]
        @MacroSetting(debug = true) def unapply(scrutinee: Any): Any = macro QuasiMacros.unapplyImpl[QC]
      }
      
      object code {
        def apply(inserted: Any*): SomeCode = macro QuasiMacros.applyImpl[QC]
        def unapply(scrutinee: SomeCode): Any = macro QuasiMacros.unapplyImpl[QC]
      }
      object dbg_code { // TODO try to refine the types..?
        @MacroSetting(debug = true) def apply(inserted: Any*): Any = macro QuasiMacros.applyImpl[QC]
        @MacroSetting(debug = true) def unapply(scrutinee: Any): Any = macro QuasiMacros.unapplyImpl[QC]
      }
      object c {
        def apply(inserted: Any*): SomeCode = macro QuasiMacros.applyImpl[QC]
        def unapply(scrutinee: SomeCode): Any = macro QuasiMacros.unapplyImpl[QC]
      }
      
    }
    
  }
  class Quasicodes[QC <: QuasiConfig] {
    val qcbase: QuasiBase.this.type = QuasiBase.this // macro expansions will make reference to it
    
    def ir[T](tree: T): Code[T, _] = macro QuasiMacros.quasicodeImpl[QC]
    @MacroSetting(debug = true) def dbg_ir[T](tree: T): Code[T, _] = macro QuasiMacros.quasicodeImpl[QC]
    
    def code[T](tree: T): Code[T, _] = macro QuasiMacros.quasicodeImpl[QC]
    @MacroSetting(debug = true) def dbg_code[T](tree: T): Code[T, _] = macro QuasiMacros.quasicodeImpl[QC]
    
    def $[T,C](q: Code[T,C]*): T = macro QuasiMacros.forward$ // to conserve the same method receiver as for QQ (the real `Base`)
    def $[A,B,C](q: Code[A,C] => Code[B,C]): A => B = macro QuasiMacros.forward$2
    //def $[T,C](q: IR[T,C]): T = macro QuasiMacros.forward$ // Actually unnecessary
    //def $[T,C](q: IR[T,C]*): T = macro QuasiMacros.forwardVararg$ // Actually unnecessary
    def $$[T](name: Symbol): T = macro QuasiMacros.forward$$
    
    type $[QT <: TypeErased] = TypeOf[QT]
    
  }
  object Quasicodes extends Quasicodes[DefaultQuasiConfig]
  object Quasiquotes extends Quasiquotes[DefaultQuasiConfig]
  
  // Note: could optimize cases with no key (currently encoded usign Unit) -- pur in a `mutable.HashMap[String,Rep]()`
  private val caches = mutable.HashMap[String,mutable.HashMap[Any,Rep]]()
  def cacheRep[K](id: String, key: K, mk: K => Rep): Rep = {
    caches.getOrElseUpdate(id,mutable.HashMap[Any,Rep]()).getOrElseUpdate(key,mk(key))
    //mk(key)  // toggle comment to see the effect of NOT caching
  }
  
}

object QuasiBase {
  
  /** Used to tag (the upper bound of) types generated for type holes, 
    * and to have a self-documenting name when the type is widened because of scope extrusion.
    * Note: now that extracted type symbols are not generated from a local `trait` but instead from the `Typ` member of a local `object`, 
    *   scope extrusion does not neatly widen extruded types to just `<extruded type>` anymore, but often things like:
    *   `Embedding.CodeType[MyClass.s.Typ]]`.
    *   Still, this trait is useful as it is used in, e.g., `QuasiTypeEmbedded` to customize the error on implicit 
    *   type not found. It's also the inferred type for things like `case List($a:$ta,$b:$tb)`, which prompts 
    *   a QQ error -- this is not really necessary, just a nicety. 
    *   Eventually we can totally get rid of that bound, as QuasiEmbedder now internally relies on `Extracted` instead. */
  type `<extruded type>`
  
}

/** Annotation used on extracted types */
class Extracted extends StaticAnnotation

object SuppressWarning {
  implicit def `scrutinee type mismatch` = Warnings.`scrutinee type mismatch`
  implicit def `use of ir instead of code` = Warnings.`use of ir instead of code`
}
object Warnings {
  // Note: these cannot be defined inside `SuppressWarning` or Scala will find them without having to import them!
  object `scrutinee type mismatch`
  object `use of ir instead of code`
}





