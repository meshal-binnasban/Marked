error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/Report/Versions/BitCoded_1.sc:indices.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/Report/Versions/BitCoded_1.sc
empty definition using pc, found symbol in pc: indices.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/s/indices.
	 -rexp/s/indices#
	 -rexp/s/indices().
	 -enumerate/s/indices.
	 -enumerate/s/indices#
	 -enumerate/s/indices().
	 -regenerate/s/indices.
	 -regenerate/s/indices#
	 -regenerate/s/indices().
	 -s/indices.
	 -s/indices#
	 -s/indices().
	 -scala/Predef.s.indices.
	 -scala/Predef.s.indices#
	 -scala/Predef.s.indices().
offset: 3547
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/Report/Versions/BitCoded_1.sc
text:
```scala
import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate, regenerate._
import $file.rebit


def fin(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
}

def mkfin(r: Rexp) : Bits = r match {
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) => if (fin(r1)) mkfin(r1) else mkfin(r2)  
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin(r1) ++ mkeps(r2)
  case SEQ(r1, r2) => mkfin(r2)
  case STAR(r) => mkfin(r) ++ List(S)
}

def mkeps(r: Rexp) : Bits = r match {
  case ONE => Nil
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) => if (nullable(r1)) Z :: mkeps(r1) else S :: mkeps(r2)  
  case SEQ(r1, r2) => mkeps(r1) ++ mkeps(r2)
  case STAR(r) =>  List(S)
}


def shift(m: Boolean, bs: Bits, r: Rexp, c: Char) : Rexp = (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case POINT(_, CHAR(d)) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case ALT(r1, r2) => ALT(shift(m, bs :+ Z, r1, c), shift(m, bs :+ S, r2, c))
  case SEQ(r1, r2) if m && nullable(r1) => SEQ(shift(m, bs, r1, c), shift(true, bs ::: mkeps(r1), r2, c))
  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs, r1, c), shift(true, mkfin(r1), r2, c))
  case SEQ(r1, r2) => SEQ(shift(m, bs, r1, c), shift(false, Nil, r2, c))
  case STAR(r) if m && fin(r) => STAR(shift(true, bs ::: (mkfin(r) :+ S), r, c))
  case STAR(r) if fin(r) => STAR(shift(true, mkfin(r) :+ Z, r, c)) 
  case STAR(r) if m => STAR(shift(m, bs:+Z, r, c))
  case STAR(r) => STAR(shift(false, Nil, r, c))
}

def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => cs.foldLeft(shift(true, Nil, r, c))((r, c) => shift(false, Nil, r, c))
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))

def lex(r: Rexp, s: List[Char]) : Option[Bits] = {
  if matcher(r, s)
  then Some(if (s == Nil) mkeps(r) else mkfin(mat(r, s)))
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

@main
def test2() = {
  println("=====Test====")
  val br2 = %("a" | "aa")
  val s = "aaa".toList
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

@main
def test3() = {
  println("=====Test====")
  val br2 = (ONE|"a") ~ ("aa" | "a")
  val s = "aa".toList
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
//
@main
def test4() = {
  println("=====Test====")
  val br2 =  %( "a" | "aa" )
  val s = "aa".toList
  println("=string=")
  println(s)
  for (i <- s.indic@@es) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  }
  println("=Marked List=") 
  println(lex(br2,s).getOrElse(Nil))
  println(s"=Derivative Bits (L=0,R=1)=")
  println(rebit.lex(br2, s))
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: indices.