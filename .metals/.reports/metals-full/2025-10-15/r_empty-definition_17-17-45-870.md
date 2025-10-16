error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/Report/Versions/BitCoded_2.sc:`<none>`.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/Report/Versions/BitCoded_2.sc
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/mkfin3.
	 -rexp/mkfin3#
	 -rexp/mkfin3().
	 -enumerate/mkfin3.
	 -enumerate/mkfin3#
	 -enumerate/mkfin3().
	 -regenerate/mkfin3.
	 -regenerate/mkfin3#
	 -regenerate/mkfin3().
	 -mkfin3.
	 -mkfin3#
	 -mkfin3().
	 -scala/Predef.mkfin3.
	 -scala/Predef.mkfin3#
	 -scala/Predef.mkfin3().
offset: 2402
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
  case ONE => List(Ep)
  case ALT(r1, r2) => if (nullable(r1)) Lf :: mkeps3(r1) else Ri :: mkeps3(r2)
  case SEQ(r1, r2) => mkeps3(r1) ++ mkeps3(r2)
  case STAR(r) => List(En)
}

def fin(r: Rexp) : Boolean = (r: @unchecked) match 
  case ZERO => false
  case ONE(bss) => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r, n) => fin(r)

def mkfin2(r: Rexp): List[Bits] = r match 
  case POINT(bss, CHAR(_)) => bss
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin3(r1) ::: mkfin3(r2)
  case ALT(r1, r2) if fin(r1) => mkfin3(r1)
  case ALT(r1, r2) if fin(r2) => mkfin3(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) && fin(r2) => mkfin3(r2) ::: mkfin3(r1).map(_ ++ (mkeps3(r2))) 
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin3(r1).map(_ ::: (mkeps3(r2))) 
  case SEQ(r1, r2) => mkfin3(r2)
  case STAR(r) => (mkfin3(r) <+> En )

  
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

  case STAR(r) if m && fin(r)=>STAR(shift(true, ((bs <+> Z)) ++ (((mk@@fin3(r))  <+> Z)) , r, c))
  case STAR(r) if fin(r) =>STAR(shift(true,(mkfin3(r) <+> Nx) , r, c))
  case STAR(r) if m =>STAR(shift(m,(bs <+> Nx) , r, c)) 
  case STAR(r) => STAR(shift(false, Nil, r, c))

  }

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s,false))

def lex(r: Rexp, s: List[Char]) : Option[List[Bits]] = {
  if matcher(r, s)
  then Some(if (s == Nil) (List(mkeps3(r))) else mkfin3(mat(r, s,true)))
  else None
}

  
```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.