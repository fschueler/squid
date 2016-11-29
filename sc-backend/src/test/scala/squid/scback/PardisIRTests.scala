package squid.scback

import ch.epfl.data.sc.pardis
import collection.mutable.ArrayBuffer

class PardisIRTests extends PardisTestSuite {
  
  import Sqd.Predef._
  import Sqd.Quasicodes._
  import Sqd.{block}
  
  
  test("Folding of Simple Expressions") {
    
    val a = ir"val x = 42; println(x); x"
    assert(stmts(a).size == 1)
    assert(ret(a) == base.const(42))
    
    val b = ir"val x = 42; val y = x; val z = y; println(z); z"
    //println(b)
    assert(stmts(b) match {
      case s :: Nil =>
        //println(s.rhs,stmts(ir{println(42)}).head.rhs)
        s.rhs == stmts(ir{println(42)}).head.rhs
      case _ => fail
    })
    assert(ret(a) == base.const(42))
    
  }
  
  
  test("Code Insertion") {
    
    def f(a: IR[Int,{}], b: IR[Int,{}]) = ir{ $(a) + $(a) + $(b) }
    
    val q = ir{ $( f(ir{1.5.toInt}, ir{666}) ) + 1 }
    //println(q)
    
    assert(stmts(q).size == 4)
    
  }
  
  
  test("Code Insertion of Effects") {
    // Note: interestingly, in Squid ir".. outer .. ${ inner } .." executes the `inner` code in the middle of the
    // construction of `outer` tree... so the statements will be directly generated and aggregated in the enclosing block,
    // unlike what happens if `inner` is built outside the quote (also tested here).
    
    val a = ir"val x = 42; println(x); x"
    //println(a)  // ir"{ val x2 = println(42); 42 }"
    assert(stmts(a).size == 1)
    
    val b0 = ir" ${ ir"val x = 42.toDouble; println(x); x" } + 1.0"
    //println(b0)  // ir"{ val x1 = 42.toDouble; val x3 = println(x1); val x5 = x1.+(1.0); x5 }"
    assert(stmts(b0).size == 3)
    
    val b1 = { val ins = ir"val x = 42.toDouble; println(x); x"; ir"$ins + 1.0" }
    //println(b1)  // ir"{ val x1 = 42.toDouble; val x3 = println(x1); val x5 = x1.+(1.0); x5 }"
    assert(stmts(b1).size == 3)
    
    val c0 = ir"println(0); ${ ir"println(1); println(2)" }; println(3); ()"
    //println(c0)  // ir"{ val x6 = println(0); val x7 = println(1); val x8 = println(2); val x9 = println(3); () }"
    assert(stmts(c0).size == 4)
    
    val c1 = { val ins = ir"println(1); println(2)"; ir"println(0); $ins; println(3); ()" }
    //println(c1)  // ir"{ val x6 = println(0); val x7 = println(1); val x8 = println(2); val x9 = println(3); () }"
    assert(stmts(c1).size == 4)
    
    val d0 = ir"println(0); ${ ir"println(1); println(2)" }"
    //println(d0)  // ir"{ val x12 = println(0); val x13 = println(1); val x14 = println(2); x14 }"
    assert(stmts(d0).size == 3)
    
    val d1 = { val ins = ir"println(1); println(2)"; ir"println(0); $ins" }
    //println(d1)  // ir"{ val x12 = println(0); val x13 = println(1); val x14 = println(2); x14 }"
    assert(stmts(d1).size == 3)
    
  }
  
  
  test("Blocks & Bindings") {
    
    val q = block(ir{ val n = 0; val a = new ArrayBuffer[Int](n); val b = 1; val c = a append 1; println(c) })
    //println(q)  // ir"{ val a2 = new ArrayBuffer[Int](0); val c5 = a2.append(1); val x7 = println(c5); x7 }"
    assert(stmts(q).size == 3)
    
  }
  
  
  //test("Blocks") {}  // TODO
  
  
  test("Subexpressions and Scheduling") {
    
    sameDefs(ir{ println(1.toDouble);println(2.toDouble) },
             ir{ val n1 = 1.toDouble; val p1 = println(n1); val n2 = 2.toDouble; val p2 = println(n2); p2 })
    
    sameDefs(ir{ val arr = new ArrayBuffer[Int](); arr append 1; val o = Option(arr.size) },
             ir{ val arr = new ArrayBuffer[Int](); val a = arr append 1; val s = arr.size; val o = Option(s); () })
    
    sameDefs(ir{ val arr = new ArrayBuffer[Int](); arr append 1; arr.size+1 },
             ir{ val arr = new ArrayBuffer[Int](); val a = arr append 1; val s = arr.size; s+1 })
    
    sameDefs(ir{ (1.toDouble+1.0,2.toDouble+2.0) },
             ir{ val n1 = 1.toDouble; val n11 = n1+1.0; val n2 = 2.toDouble; val n22 = n2+2.0; Tuple2(n11,n22) })
    
  }
  
  
  test("By-Name Parameters") {
    
    val t0 = ir"Option(42) getOrElse {println(0); 666}"
    assert(stmts(t0).size == 2)
    assert(stmts(t0)(1).rhs.funArgs(1).asInstanceOf[pardis.ir.PardisBlock[_]].stmts.size == 1)
    
    val t1 = ir"Option(()) getOrElse {val r = println(0); r}"
    assert(stmts(t1).size == 2)
    
    val t2 = ir""" Option[String](null) getOrElse "42" """
    assert(stmts(t2).size == 2)
    
    //println(ir"None") // There is no auto-binding for this (in fact, not even a mirror defined for it in SC's ScalaCore)
    //println(ir"None getOrElse 42") // Note: this will pass the block for `42` as the first argument to the generic Option's getOrElse, and result in a cast crash...
    
    // Note: also test Option.empty when it is in SC
    
  }
  
  
  test("Types") {
    
    val ABI = SC.typeArrayBuffer(SC.typeInt)
    
    assert(ir{ new ArrayBuffer[Int] }.typ.rep == ABI)
    
    assert(ir{ val n1 = 1.toDouble.toInt; ArrayBuffer(n1) }.typ.rep == ABI)
    
    assert(stmts(ir{ val n1 = 1.toDouble }).head.typeT == typeRepOf[Double])
    
  }
  
  
}
