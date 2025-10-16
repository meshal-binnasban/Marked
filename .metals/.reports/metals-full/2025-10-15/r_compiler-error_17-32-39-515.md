file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/Report/Versions/BitCoded_2.sc
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 2797
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
  case ALT(r1, r2) => if (nullable(r1)) Z :: mkeps2(r1) else S :: mkeps2(r2)
  case SEQ(r1, r2) => mkeps2(r1) ++ mkeps2(r2)
  case STAR(r) => List(S)
}

def fin(r: Rexp) : Boolean = (r: @unchecked) match 
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)

def mkfin2(r: Rexp): List[Bits] = r match 
  case POINT(bss, CHAR(_)) => bss
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin2(r1) ::: mkfin2(r2)
  case ALT(r1, r2) if fin(r1) => mkfin2(r1)
  case ALT(r1, r2) if fin(r2) => mkfin2(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) && fin(r2) => mkfin2(r2) ::: mkfin2(r1).map(_ ++ (mkeps2(r2))) 
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin2(r1).map(_ ::: (mkeps2(r2))) 
  case SEQ(r1, r2) => mkfin2(r2)
  case STAR(r) => (mkfin2(r) <+> S )

  
def shift(m: Boolean, bs: List[Bits], r: Rexp, c: Char) : Rexp = 
  (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c)  POINT((bs) , CHAR(d)) else CHAR(d)
  case POINT(bss, CHAR(d)) => if (m && d == c) POINT((bs)  , CHAR(d)) else CHAR(d) 
  case ALT(r1, r2) => ALT(shift(m, (bs <+>Z)   , r1, c), shift(m, (bs <+> S)    , r2, c)) 

  case SEQ(r1, r2) if m && nullable(r1) && fin(r1) => 
    SEQ(shift(m, bs , r1, c), shift(true, ((mkfin2(r1))) ++ ((bs <++> mkeps2(r1))), r2, c)) 
  case SEQ(r1, r2) if m && nullable(r1)=> 
    SEQ(shift(m, bs  , r1, c), shift(true, ((bs <++> mkeps2(r1)))  , r2, c)) 

  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs , r1, c), shift(true, (mkfin2(r1) ), r2, c)) 
  case SEQ(r1, r2) => SEQ(shift(m, bs  , r1, c), shift(false, Nil, r2, c)) 

  case STAR(r) if m && fin(r)=>STAR(shift(true, ((bs <+> Z)) ++ (((mkfin2(r))  <+> Z)) , r, c))
  case STAR(r) if fin(r) =>STAR(shift(true,(mkfin2(r) <+> Z) , r, c))
  case STAR(r) if m =>STAR(shift(m,(bs <+> Z) , r, c)) 
  case STAR(r) => STAR(shift(false, Nil, r, c))

  }

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))

def mat(r: Rexp, s: List[Char]): Rexp = s match 
  case Nil => r
  case c :: cs => cs.foldLeft(shift(true, List(), r, c))((r, c) => shift(false, List(@@), r, c))


def lex(r: Rexp, s: List[Char]) : Option[List[Bits]] = {
  if matcher(r, s)
  then Some(if (s == Nil) (List(mkeps2(r))) else mkfin2(mat(r, s)))
  else None
}

@main
def test1() = {
  println("=====Test====")
  val br2 = ("ab") | ("ba")
  val s = "ba".toList
  println("=string=")
  println(s)
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 
  
  println("=Marked List=") 
  println(lex(br2,s).getOrElse(Nil))
  println(s"=Derivative Bits (L=0,R=1)=")
  println(rebit.lex(br2, s))
}
  
```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:129)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:244)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:101)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:88)
	dotty.tools.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:46)
	dotty.tools.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:435)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: -1