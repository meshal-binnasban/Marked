file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_explicit_bits_2.sc
### java.lang.IndexOutOfBoundsException: -1

occurred in the presentation compiler.

presentation compiler configuration:


action parameters:
offset: 8499
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_explicit_bits_2.sc
text:
```scala
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
  case ONE => List(E) 
  case POINT(bs,CHAR(c)) => bs
  case ALT(r1, r2) => if (nullable(r1)) Z :: mkeps(r1) else S :: mkeps(r2)  
  case SEQ(r1, r2) => (SE1::mkeps(r1)) ::: (SE2 :: mkeps(r2) ) // if nullable r1 add 2?
  case STAR(r) =>List(ST2)
  //case CHAR(_) => Nil //for testing mkeps outside lex
}

def mkfin(r: Rexp) : Bits = (r: @unchecked) match {
  case POINT(bs,CHAR(_)) => bs
  case ALT(r1, r2) => 
/*     if (fin(r1) && fin(r2)) {
      println(s"mkfin1=${mkfin(r1)}, mkfin2=${mkfin(r2)}\ntotalBitsWeight r1 ${totalBitsWeight(mkfin(r1))} and r2 ${totalBitsWeight(mkfin(r2))}")
      if (totalBitsWeight(mkfin(r1))>=totalBitsWeight(mkfin(r2)))
      mkfin(r1) else mkfin(r2)
      
    } else  */if (fin(r1)) mkfin(r1) else mkfin(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => (mkfin(r1)) ::: (SE2 :: mkeps(r2) ) // added SE2 for cases of null r2 not having 3/SE2 flag
  case SEQ(r1, r2) => mkfin(r2) 

  case STAR(r) =>  (mkfin(r)) ++ List(ST2)  //ST1:: 
  case NTIMES(r: Rexp, n: Int) => mkfin(r) 
}

def mkfin2(r: Rexp) : Set[Bits] = r match {
  case POINT(bs, CHAR(_)) => Set(bs)
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin2(r1) | mkfin2(r2)
  case ALT(r1, r2) if fin(r1) => mkfin2(r1)
  case ALT(r1, r2) if fin(r2) => mkfin2(r2)                     
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin2(r1).map(_ ++ mkeps(r2))
  case SEQ(r1, r2) => mkfin2(r2)
  case STAR(r) => mkfin2(r).map(_ ++ List(S))
}

def lex2(r: Rexp, s: List[Char]) : Option[Set[Bits]] = {
  if matcher(r, s)
  then Some(if (s == Nil) Set(mkeps(r)) else mkfin2(mat(r, s)))
  else None
}

// shift function from the paper
def shift(m: Boolean, bs: Bits, r: Rexp, c: Char) : Rexp = (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT(bs:+C,CHAR(d)) else CHAR(d)
  case POINT(bits,CHAR(d)) => if (m && d == c) POINT(bs:+C,CHAR(d)) else CHAR(d)

  case ALT(r1, r2) => ALT(shift(m, bs:+Z , r1, c), shift(m, bs:+S, r2, c))

  case SEQ(r1, r2) if m && nullable(r1) =>
     SEQ(shift(m, bs:+SE1, r1, c), shift(true,  bs::: (SE1::((mkeps(r1)):+SE2)), r2, c)) 
  
  case SEQ(r1, r2) if fin(r1) =>
     SEQ(shift(m, bs:+SE1, r1, c), shift(true, ((mkfin(r1))):+SE2, r2, c))
  
  case SEQ(r1, r2) => SEQ(shift(m, bs:+SE1, r1, c), shift(false, Nil, r2, c)) //Nil
  
  case STAR(r) if m && fin(r) =>STAR(shift(true, (bs) :+ST1, r, c)) //273-     2732 , 274537
  case STAR(r) if fin(r) => STAR(shift(true, (mkfin(r):+ST1), r, c)) 
  case STAR(r) if m => STAR(shift(m, bs:+ST1, r, c))
  case STAR(r) => STAR(shift(false, Nil, r, c))

  case NTIMES(r,n) if m && fin(r) =>NTIMES(shift(true, (bs) :+ST1, r, c) , n) //273-     2732 , 274537
  case NTIMES(r,n) if fin(r) => NTIMES(shift(true, (mkfin(r):+ST1), r, c) , n) 
  case NTIMES(r,n) if m => NTIMES(shift(m, bs:+ST1, r, c),n)
  case NTIMES(r,n) => NTIMES(shift(false, Nil, r, c),n)
}

def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => cs.foldLeft(shift(true, Nil, r, c))((r, c) => shift(false, Nil, r, c))
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
    if (s == Nil) nullable(r) else fin(mat(r, s))

def matcher2(r: Rexp, s: List[Char]) : Rexp =
    if (s == Nil){ if(nullable(r)) r else ZERO}
    else mat(r, s)

def lex(r: Rexp, s: List[Char]) : Option[Bits] = {
  if matcher(r, s)
  then Some( if (s == Nil) mkeps(r) else mkfin(mat(r, s)))
  else None
}

def mDecode(bs: Bits, r: Rexp): (VALUE, Bits) =  (r: @unchecked) match {
  case ONE => (bs: @unchecked) match {
    case E :: rest => (EMPTY, rest)
    case _ => (ERRORVALUE(s"ONE ERROR, bs=$bs"), bs)
  } 
  case CHAR(c) => (bs: @unchecked) match {
    case C :: rest => (CHARV(c), rest)
  } 
  case ALT(r1, r2) => (bs: @unchecked) match {
    case Z :: rest => 
      val (v, rem) = mDecode(rest, r1)
      (LEFT(v), rem)
    case S :: rest => 
      val (v, rem) = mDecode(rest, r2)
      (RIGHT(v), rem)
    case _ => (ERRORVALUE(s"ALT ERROR, bs=$bs"), bs)
    }
  case SEQ(r1, r2) => (bs: @unchecked) match {
    case SE1 :: rest =>
      val (v1, bs1) = mDecode(rest,r1)
      (bs1: @unchecked) match {
        case SE2 :: rest2 =>
          val (v2, bs2) = mDecode(rest2,r2)
          (SEQV(v1, v2), bs2)
        case _ => (ERRORVALUE(s"SEQ ERROR, bs=$bs"), Nil)
      }
    }
  case STAR(r) => bs match {
    case ST2 :: rest => (STARV(Nil), rest) // STAR end
    case ST1 :: rest =>
      val (v, bs1) = mDecode(rest, r)                    
      val (vs, bs2) = mDecode(bs1, STAR(r))              
      vs match {
        case STARV(vs) => (STARV(v :: vs), bs2)
        case _ => (ERRORVALUE(s"STAR ERROR inside, bs=$bs1"), bs1)
    }
  case _ =>
    (ERRORVALUE(s"STAR ERROR, bs=$bs"), bs)
  }
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
  case (STARV(vs1), STARV(vs2)) => 
      vs1.zip(vs2).forall { case (x, y) => compareResults(x, y) } 
  case (ERRORVALUE(_), ERRORVALUE(_)) => true 
  case _ => false 
}

def bdecode(bs: Bits): (VALUE, Bits) = bs match {
  case C :: rest => (CHARV('x'), rest) // placeholder value, maybe include empty to imitate the derivatives approach?
  case E :: rest => (EMPTY, rest)

  //ALT case; Z=0 Left and S=1 Right
  case Z :: rest => 
    val (v1, bs1) = bdecode(rest)
    (LEFT(v1), bs1)
  case S :: rest => 
    val (v2, bs2) = bdecode(rest)
    (RIGHT(v2), bs2)

  //SEQ case; SE1=0 r1 and SE2=1  r2
  case SE1 :: rest => 
    val (v1, bs1) = bdecode(rest)
    bs1 match {
      case SE2 :: bs2 =>
        val (v2, bs3) = bdecode(bs2)
        (SEQV(v1, v2), bs3)
      case _ => (ERRORVALUE(s"SEQ ERROR, bs=$bs"), bs)
    }

  //STAR case; ST1=0 r start and ST2=1 end  
  case ST1 :: rest => 
    val (v, bs1) = bdecode(rest)
    val (svs, bs2) = bdecode(bs1)
    svs match {
      case STARV(vs) => (STARV(v :: vs), bs2)
      case _         => (ERRORVALUE(s"STAR 1 ERROR bs=$bs2"), bs2)
    }
  case ST2 :: rest => (STARV(Nil), rest)
  case _ =>
    (ERRORVALUE(s"unmatched bitcode: ${bs}"), bs)
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

  println(s"===============\nMarked bitcode: ${bits}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\nComparing Values using compareResults = ${compareResults(markedValue, derivValue)}")
  println(s"Marked mdecode value == Derivative value :  ${(markedValue == derivValue)}")

  val onlyBitsValue=bdecode(bits)._1
  println(s"===============\nDecoded value without regex, bdecode=${onlyBitsValue}")
  println(s"bdecode compareResults mdecode : ${compareResults(markedValue,onlyBitsValue)}")

  val bitsSet=lex2(rexp,s).getOrElse(Set())
  for(@@)
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

  println(s"===============\nMarked bitcode: ${bits}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\nComparing Values using compareResults = ${compareResults(markedValue, derivValue)}")
  println(s"Marked mdecode value == Derivative value :  ${(markedValue == derivValue)}")

  val onlyBitsValue=bdecode(bits)._1
  println(s"===============\nDecoded value without regex, bdecode=${onlyBitsValue}")
  println(s"bdecode compareResults mdecode : ${compareResults(markedValue,onlyBitsValue)}")
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

  println(s"===============\nMarked bitcode: ${bits}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\nComparing Values using compareResults = ${compareResults(markedValue, derivValue)}")
  println(s"Marked mdecode value == Derivative value :  ${(markedValue == derivValue)}")

  val onlyBitsValue=bdecode(bits)._1
  println(s"===============\nDecoded value without regex, bdecode=${onlyBitsValue}")
  println(s"bdecode compareResults mdecode : ${compareResults(markedValue,onlyBitsValue)}")
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

  println(s"===============\nMarked bitcode: ${bits}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\nComparing Values using compareResults = ${compareResults(markedValue, derivValue)}")
  println(s"Marked mdecode value == Derivative value :  ${(markedValue == derivValue)}")

  val onlyBitsValue=bdecode(bits)._1
  println(s"===============\nDecoded value without regex, bdecode=${onlyBitsValue}")
  println(s"bdecode compareResults mdecode : ${compareResults(markedValue,onlyBitsValue)}")
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

  println(s"===============\nMarked bitcode: ${bits}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\nComparing Values using compareResults = ${compareResults(markedValue, derivValue)}")
  println(s"Marked mdecode value == Derivative value :  ${(markedValue == derivValue)}")

  val onlyBitsValue=bdecode(bits)._1
  println(s"===============\nDecoded value without regex, bdecode=${onlyBitsValue}")
  println(s"bdecode compareResults mdecode : ${compareResults(markedValue,onlyBitsValue)}")
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

  println(s"===============\nMarked bitcode: ${bits}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\nComparing Values using compareResults = ${compareResults(markedValue, derivValue)}")
  println(s"Marked mdecode value == Derivative value :  ${(markedValue == derivValue)}")

  val onlyBitsValue=bdecode(bits)._1
  println(s"===============\nDecoded value without regex, bdecode=${onlyBitsValue}")
  println(s"bdecode compareResults mdecode : ${compareResults(markedValue,onlyBitsValue)}")
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

  val finReg=matcher2(rexp,s)
  val bits=lex(rexp, s).getOrElse(Nil)
  val markedValue=mDecode(bits, rexp)._1

  println(s"===============\nFinal Reg:\n ${pp(finReg)} \nnullable=${nullable(finReg)}")
  println(s"===============\nMarked bitcode: ${bits} \nMarked bitcode converted: ${convertMtoDBit2(bits)}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1

  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"===============\nmarked bitcode == derivatives bitcode :  ${(bitsToInts(convertMtoDBit2(bits)) == derivBitcode)}")

  val onlyBitsValue=bdecode(bits)._1
  println(s"===============\nDecoded value without regex, bdecode=${onlyBitsValue}")
  println(s"bdecode compareResults derivative value : ${compareResults(derivValue,onlyBitsValue)}")
}

def pp(e: Rexp) : String = (e: @unchecked) match {
  case ZERO => "0\n"
  case ONE => s"1\n"
  case CHAR(d) => s"$d \n"
  case POINT(bs,CHAR(c)) => s"${"\u001b[32m"} •$c :${bs.mkString(",")} ${"\u001b[0m"}\n" 
  case ALT(r1, r2) => s"ALT \n" ++ pps(r1, r2)
  case SEQ(r1, r2) => s"SEQ \n" ++ pps(r1, r2)
  case STAR(r) => s"STAR \n" ++ pps(r)
  case INIT(r) => s"INIT\n" ++ pps(r)
}
def pps(es: Rexp*) = indent(es.map(pp))
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