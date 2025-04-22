import scala.language.implicitConversions
import $file.rexp, rexp._, rexp.Rexp._, rexp.VALUE._
import $file.derivativesBitcode, derivativesBitcode._

def nullable(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => true
  case CHAR(d) =>  false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(r) => true
  case POINT(bs,r) => nullable(r)
  case NTIMES(r: Rexp, n: Int) => if (n == 0) true else nullable(r)
}
def fin(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(bs,CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r,n) => fin(r) // count occurences vs n?
}

def mkeps(r: Rexp) : Bits = (r: @unchecked) match {
  case ONE => Nil
  case POINT(bs,CHAR(c)) => bs
  case ALT(r1, r2) => if (nullable(r1)) Z :: mkeps(r1) else S :: mkeps(r2)  
  case SEQ(r1, r2) => (mkeps(r1)) ::: (mkeps(r2) ) 
  case STAR(r) => List(S)
}

def mkfin(r: Rexp) : Bits = (r: @unchecked) match {
  case POINT(bs,CHAR(_)) => bs
  case ALT(r1, r2) => 
    if (fin(r1)) mkfin(r1) else mkfin(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => (mkfin(r1)) ::: (mkeps(r2) ) 
  case SEQ(r1, r2) => mkfin(r2) 

  case STAR(r) =>  mkfin(r) ++ List(S) 
  case NTIMES(r: Rexp, n: Int) => mkfin(r) 
}

def mkfin2(r: Rexp) : Set[Bits] = (r: @unchecked) match {
  case POINT(bs, CHAR(_)) => Set(bs)
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin2(r1) | mkfin2(r2)
  case ALT(r1, r2) if fin(r1) => mkfin2(r1)
  case ALT(r1, r2) if fin(r2) => mkfin2(r2)                     
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin2(r1).map(_ ++ mkeps(r2))
  case SEQ(r1, r2) => mkfin2(r2)
  case STAR(r) => mkfin2(r).map(_ ++ List(S))
}

def shift(m: Boolean, bs: Bits, r: Rexp, c: Char) : Rexp = (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT(bs,CHAR(d)) else CHAR(d)
  case POINT(bits,CHAR(d)) => if (m && d == c) POINT(bs,CHAR(d)) else CHAR(d)

  case ALT(r1, r2) => ALT(shift(m, bs:+Z , r1, c), shift(m, bs:+S, r2, c))

  case SEQ(r1, r2) if m && nullable(r1) =>
     SEQ(shift(m, bs, r1, c), shift(true,  bs ::: mkeps(r1), r2, c)) 
  
  case SEQ(r1, r2) if fin(r1) =>
     SEQ(shift(m, bs, r1, c), shift(true, mkfin(r1), r2, c))
  
  case SEQ(r1, r2) => SEQ(shift(m, bs, r1, c), shift(false, Nil, r2, c)) 
  
  case STAR(r) if m && fin(r) =>STAR(shift(true, (bs) :+Z, r, c)) 
  case STAR(r) if fin(r) => STAR(shift(true, (mkfin(r):+Z), r, c)) 
  case STAR(r) if m => STAR(shift(m, bs:+Z, r, c))
  case STAR(r) => STAR(shift(false, Nil, r, c))
}

def mDecode(bs: Bits, r: Rexp): (VALUE, Bits) =  (r: @unchecked) match {
  case ONE => (EMPTY, bs)
  case CHAR(c) => (CHARV(c), bs)
  case ALT(r1, r2) => (bs: @unchecked) match {
    case Z :: rest => 
      val (v, rem) = mDecode(rest, r1)
      (LEFT(v), rem)
    case S :: rest => 
      val (v, rem) = mDecode(rest, r2)
      (RIGHT(v), rem)
    }
  case SEQ(r1, r2) => 
      val (v1, bs1) = mDecode(bs,r1)
      val (v2, bs2) = mDecode(bs1,r2)
      (SEQV(v1, v2), bs2)

  case STAR(r) => (bs: @unchecked) match {
    case S :: rest => (STARV(Nil), rest) // STAR end
    case Z :: rest =>
      val (v, bs1) = mDecode(rest, r)                    
      val (vs, bs2) = mDecode(bs1, STAR(r))              
      (vs: @unchecked) match {
        case STARV(vs) => (STARV(v :: vs), bs2)
    }
  }
  case _ => (ERRORVALUE(s"ERROR, r=$r, bs=$bs"), bs)
}

def compareResults(v1:VALUE, v2:VALUE): Boolean = (v1, v2) match {
  case (EMPTY, EMPTY) => true
  case (CHARV(c1), CHARV(c2)) => c1 == c2 //true if using bdecode
  case (SEQV(a1, b1), SEQV(a2, b2)) =>
    compareResults(a1, a2) && compareResults(b1, b2)
  case (LEFT(vl1), LEFT(vl2)) =>
    compareResults(vl1, vl2)
  case (RIGHT(vr1), RIGHT(vr2)) =>
    compareResults(vr1, vr2)
  case (STARV(vs1), STARV(vs2))           =>
    vs1.length == vs2.length &&
      vs1.zip(vs2).forall { case (x, y) => compareResults(x, y) } 
  case (ERRORVALUE(_), ERRORVALUE(_)) => true 
  case _ => false 
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

def lex2(r: Rexp, s: List[Char]) : Option[Set[Bits]] = {
  if matcher(r, s)
  then Some(if (s == Nil) Set(mkeps(r)) else mkfin2(mat(r, s)))
  else None
}


// testing (1 | "c") ~ ("cc" | "c") regex 
@main
def test1() = {
  println("=====Test With ONE====")
  val rexp=SEQ( ALT(ONE, CHAR('c')) ,ALT(SEQ(CHAR('c'),CHAR('c')), CHAR('c')) )
  println(s"regex= $rexp")
  val s = "cc".toList

  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 

  val bits=lex(rexp, s).getOrElse(Nil)
  val markedValue=mDecode(bits, rexp)._1

  println(s"===============\nMarked bitcode: ${bits} \nMarked bitcode converted: ${convertMtoDBit2(bits)}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"===============\nmarked bitcode == derivatives bitcode :  ${(bitsToInts(convertMtoDBit2(bits)) == derivBitcode)}")

  val bitsSet=lex2(rexp,s).getOrElse(Set())
  for( b <- bitsSet) {
    println(s"bitsSet=${b}")
  }

}

// testing (("a" | "b") | ("ab") ) | ("bc" | ("c"| "b"))  regex 
@main
def test2() = {
  println("=====Test With SEQ/ALT/CHAR only====")
  val rexp=SEQ(
    ALT(ALT(CHAR('a'),CHAR('b')),SEQ(CHAR('a'),CHAR('b'))) , 
    ALT( SEQ(CHAR('b'),CHAR('c')), ALT(CHAR('c'),CHAR('b'))) ) 
  println(s"regex= $rexp")
  val s = "abc".toList
  
  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 

  val bits=lex(rexp, s).getOrElse(Nil)
  val markedValue=mDecode(bits, rexp)._1

  println(s"===============\nMarked bitcode: ${bits} \nMarked bitcode converted: ${convertMtoDBit2(bits)}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"===============\nmarked bitcode == derivatives bitcode :  ${(bitsToInts(convertMtoDBit2(bits)) == derivBitcode)}")

}

// testing "ac" | ("c"|"c") regex
@main
def test3() = {
  println("=====Test====")
  //val rexp=("a" | "ab") ~ ("b" | ONE)
  //val rexp= ONE | ONE~"c"
  //val rexp= ONE | ("a"|ONE)~"c"
  val rexp="ac" | ("c"|"c")
  println(s"regex= $rexp")
  val s = "c".toList
  
  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 

  val bits=lex(rexp, s).getOrElse(Nil)
  val markedValue=mDecode(bits, rexp)._1

  println(s"===============\nMarked bitcode: ${bits} \nMarked bitcode converted: ${convertMtoDBit2(bits)}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"===============\nmarked bitcode == derivatives bitcode :  ${(bitsToInts(convertMtoDBit2(bits)) == derivBitcode)}")

}

// testing ("a"| ONE | ONE) | (("b" | "c") | "c") regex
@main
def test4() = {
  println("=====Test====")
  val rexp= ("a"| ONE | ONE) | (("b" | "c") | "c")
  println(s"regex= $rexp")
  val s = "c".toList
  
  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 

  val bits=lex(rexp, s).getOrElse(Nil)
  val markedValue=mDecode(bits, rexp)._1

  println(s"===============\nMarked bitcode: ${bits} \nMarked bitcode converted: ${convertMtoDBit2(bits)}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"===============\nmarked bitcode == derivatives bitcode :  ${(bitsToInts(convertMtoDBit2(bits)) == derivBitcode)}")

}

// testing ("a"| "ab") | (("c"|"a") ~ ("b" | "c") ) regex
@main
def test5() = {
  println("=====Test====")
  val rexp= ("a"| "ab") | (("c"|"a") ~ ("b" | "c") )
  println(s"regex= $rexp")
  val s = "ab".toList
  
  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 

  val bits=lex(rexp, s).getOrElse(Nil)
  val markedValue=mDecode(bits, rexp)._1

  println(s"===============\nMarked bitcode: ${bits} \nMarked bitcode converted: ${convertMtoDBit2(bits)}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"===============\nmarked bitcode == derivatives bitcode :  ${(bitsToInts(convertMtoDBit2(bits)) == derivBitcode)}")

}

// testing (ONE | "a")  | ((ONE ~ ONE) ~ "a") regex
@main
def test6() = {
  println("=====Test====")
  val rexp= (ONE | "a")  | ((ONE ~ ONE) ~ "a")
  println(s"regex= $rexp")
  val s = "a".toList
  
  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 

  val bits=lex(rexp, s).getOrElse(Nil)
  val markedValue=mDecode(bits, rexp)._1

  println(s"===============\nMarked bitcode: ${bits} \nMarked bitcode converted: ${convertMtoDBit2(bits)}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"===============\nmarked bitcode == derivatives bitcode :  ${(bitsToInts(convertMtoDBit2(bits)) == derivBitcode)}")

}

// testing (ONE | ("a" | "a")) | (ONE ~ "b" | ("b"|"a")) regex
@main
def test7() = {
  println("=====Test====")
  //val rexp= STAR(STAR("c"))
  //ALT(STAR(STAR(CHAR(a))),ALT(ALT(CHAR(c),ONE),ALT(CHAR(a),CHAR(b))))
  val rexp=ALT(ALT(ONE,ALT(CHAR('a'),CHAR('a'))),ALT(SEQ(ONE,CHAR('b')),ALT(CHAR('b'),CHAR('a'))))
  //val rexp=(ONE | ("a" | "a")) | (ONE ~ "b" | ("b"|"a"))
  println(s"regex= $rexp")
  val s = "b".toList
  
  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 

  val bits=lex(rexp, s).getOrElse(Nil)
  val markedValue=mDecode(bits, rexp)._1

  println(s"===============\nMarked bitcode: ${bits} \nMarked bitcode converted: ${convertMtoDBit2(bits)}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"===============\nmarked bitcode == derivatives bitcode :  ${(bitsToInts(convertMtoDBit2(bits)) == derivBitcode)}")

}

// testing STAR("a") regex
@main
def test8() = {
  println("=====Test====")
  val rexp=STAR("a")
  println(s"regex= $rexp")
  
  val s = "a".toList
  
  println("=string=")
  println(s)
  
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(rexp, sPart)))
  } 

  val bits=lex(rexp, s).getOrElse(Nil)
  val markedValue=mDecode(bits, rexp)._1

  println(s"===============\nMarked bitcode: ${bits} \nMarked bitcode converted: ${convertMtoDBit2(bits)}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"===============\nmarked bitcode == derivatives bitcode :  ${(bitsToInts(convertMtoDBit2(bits)) == derivBitcode)}")

}

def pp(e: Rexp) : String = (e: @unchecked) match {
  case ZERO => "0\n"
  case ONE => s"1\n"
  case CHAR(d) => s"$d \n"
  case POINT(bs,CHAR(c)) => s"${"\u001b[32m"} •$c :${bs.mkString(",")} ${"\u001b[0m"}\n" 
  case ALT(r1, r2) => s"ALT \n" ++ pps(r1, r2)
  case SEQ(r1, r2) => s"SEQ \n" ++ pps(r1, r2)
  case STAR(r) => s"STAR \n" ++ pps(r)
  //case INIT(r) => s"INIT\n" ++ pps(r)
}
def pps(es: Rexp*) = indent(es.map(pp))


