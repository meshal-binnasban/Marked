file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/Report/Versions/BitCoded_2.sc
### java.lang.AssertionError: NoDenotation.owner

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/Report/Versions/BitCoded_2.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit


extension (xs: List[Bits]) {
  def <*> (ys: List[Bits]): List[Bits] =
    for (x <- xs; y <- ys) yield x ::: y

  def <+> (y: Bit): List[Bits] =
    for (x <- xs) yield x :+ y

  def <++>(ys: Bits): List[Bits] =
    xs.map(_ ++ ys) 

  def <::>(y: Bit): List[Bits] = 
    for (x <- xs) yield y :: x
}

def mkeps2(r: Rexp) : Bits = r match {
  case ONE => Nil
  case ALT(r1, r2) => if (nullable(r1)) Z :: mkeps3(r1) else S :: mkeps3(r2)
  case SEQ(r1, r2) => mkeps3(r1) ++ mkeps3(r2)
  case STAR(r) => List(S)
}

def fin(r: Rexp) : Boolean = (r: @unchecked) match 
  case ZERO => false
  case ONE(bss) => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)

def mkfin2(r: Rexp): List[Bits] = r match 
  case POINT(bss, CHAR(_)) => bss
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin3(r1) ::: mkfin3(r2)
  case ALT(r1, r2) if fin(r1) => mkfin3(r1)
  case ALT(r1, r2) if fin(r2) => mkfin3(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) && fin(r2) => mkfin3(r2) ::: mkfin3(r1).map(_ ++ (mkeps3(r2))) 
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin3(r1).map(_ ::: (mkeps3(r2))) 
  case SEQ(r1, r2) => mkfin3(r2)
  case STAR(r) => (mkfin3(r) <+> S )

  
def shift(m: Boolean, bs: List[Bits], r: Rexp, c: Char) : Rexp = 
  (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c)  POINT((bs <+> Ch) , CHAR(d)) else CHAR(d)
  case POINT(bss, CHAR(d)) => if (m && d == c) POINT((bs <+> Ch)  , CHAR(d)) else CHAR(d) 
  case ALT(r1, r2) => ALT(shift(m, (bs <+>Z)   , r1, c), shift(m, (bs <+> S)    , r2, c)) 

  case SEQ(r1, r2) if m && nullable(r1) && fin(r1) => 
    SEQ(shift(m, bs , r1, c), shift(true, ((mkfin2(r1))) ++ ((bs <++> mkeps2(r1))), r2, c)) 
  case SEQ(r1, r2) if m && nullable(r1)=> 
    SEQ(shift(m, bs  , r1, c), shift(true, ((bs <++> mkeps2(r1)))  , r2, c)) 

  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs , r1, c), shift(true, (mkfin2(r1) ), r2, c)) 
  case SEQ(r1, r2) => SEQ(shift(m, bs  , r1, c), shift(false, Nil, r2, c)) 

  case STAR(r) if m && fin(r)=>STAR(shift(true, ((bs <+> Z)) ++ (((mkfin3(r))  <+> Z)) , r, c))
  case STAR(r) if fin(r) =>STAR(shift(true,(mkfin3(r) <+> Z) , r, c))
  case STAR(r) if m =>STAR(shift(m,(bs <+> Z) , r, c)) 
  case STAR(r) => STAR(shift(false, Nil, r, c))

  }

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s,false))

def mat(r: Rexp, s: List[Char], prnt: Boolean = false): Rexp = s match 
  case Nil => r
  case c :: cs =>
        val initialMarked = shift(true, List(List()), r, c)
    val initialDer = rebit.bder(c, rebit.intern(erase(r)))
    if (prnt)
      /* if(nullablePoint(initialMarked)) 
        println(s"Shift - $c: \n${pp(initialMarked)}\n mkepsPoint= ${mkepsPoint(initialMarked)}")
      else 
        println(s"Shift - $c: - no points: \n${pp(initialMarked)}\n") */
      matPrintHelper(c, initialMarked, initialDer) 
    val (finalMarked, finalDerivative) = cs.foldLeft((initialMarked, initialDer)) {
      case ((r, d), ch) =>
        val nextMarked = shift(false, List(List()), r, ch)
        val nextDer = rebit.bder(ch, d)
        if (prnt)
         /*  if(nullablePoint(nextMarked)) 
          println(s"Shift - $ch: \n${pp(nextMarked)}\n mkepsPoint= ${mkepsPoint(nextMarked)}")
          else 
            println(s"Shift - $ch: - no points: \n${pp(nextMarked)}\n") */
          matPrintHelper(ch, nextMarked, nextDer) 
        (nextMarked, nextDer)
    }
    if (prnt) {
      println(s"Final Marked Regex: \n${pp(finalMarked)}")
      println(s"Final Derivative: \n${rebit.pps(finalDerivative)}")
    }
    finalMarked
def lex(r: Rexp, s: List[Char]) : Option[List[Bits]] = {
  if matcher(r, s)
  then Some(if (s == Nil) (List(mkeps3(r))) else mkfin3(mat(r, s,true)))
  else None
}

  
```



#### Error stacktrace:

```
dotty.tools.dotc.core.SymDenotations$NoDenotation$.owner(SymDenotations.scala:2623)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.isSelfSym(SymDenotations.scala:718)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:330)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.fold$1(Trees.scala:1665)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.apply(Trees.scala:1667)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1698)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:457)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1706)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:457)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.fold$1(Trees.scala:1665)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.apply(Trees.scala:1667)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1704)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:457)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse$$anonfun$13(ExtractSemanticDB.scala:391)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:334)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:386)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:348)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.fold$1(Trees.scala:1665)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.apply(Trees.scala:1667)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1698)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:457)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1706)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:457)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.fold$1(Trees.scala:1665)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.apply(Trees.scala:1667)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1704)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:457)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1753)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:354)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse$$anonfun$11(ExtractSemanticDB.scala:377)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:334)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:377)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.apply(Trees.scala:1801)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1757)
	dotty.tools.dotc.ast.Trees$Instance$TreeAccumulator.foldOver(Trees.scala:1671)
	dotty.tools.dotc.ast.Trees$Instance$TreeTraverser.traverseChildren(Trees.scala:1802)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:351)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse$$anonfun$1(ExtractSemanticDB.scala:315)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:334)
	dotty.tools.dotc.semanticdb.ExtractSemanticDB$Extractor.traverse(ExtractSemanticDB.scala:315)
	dotty.tools.pc.SemanticdbTextDocumentProvider.textDocument(SemanticdbTextDocumentProvider.scala:36)
	dotty.tools.pc.ScalaPresentationCompiler.semanticdbTextDocument$$anonfun$1(ScalaPresentationCompiler.scala:242)
```
#### Short summary: 

java.lang.AssertionError: NoDenotation.owner