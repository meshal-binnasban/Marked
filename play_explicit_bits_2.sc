import scala.language.implicitConversions
import $file.rexp, rexp._, rexp.Rexp._, rexp.VALUE._
import $file.derivativesBitcode, derivativesBitcode._
import scala.math.Ordering.Implicits._

// nullable 
def nullable(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => true
  case CHAR(d) =>  false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(r) => true
  case POINT(bs,r) => nullable(r)
  case INIT(r) => nullable(r) // added to check nullable input
}
// fin function from the paper
// checks whether a mark is in "final" position
def fin(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => false
  case CHAR(_) => false
  case POINT(bs,CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case INIT(r) => fin(r) // added to check nullable input
}

def mkeps(r: Rexp) : Bits = (r: @unchecked) match {
  case ONE => List(E) //8::bs // flag added to indicate a regex used empty string to match 
  case POINT(bs,CHAR(c)) => bs
  case ALT(r1, r2) => 
    if (nullable(r1)) Z :: mkeps(r1) else S :: mkeps(r2)  
  case SEQ(r1, r2) => (SE1::mkeps(r1)) ::: (SE2 :: mkeps(r2) ) // if nullable r1 add 2?
  
  case STAR(r) =>List(ST2)
    //if(nullable(r))(mkeps(r)):+ST2 else List(ST2)
    //if(nullable(r)) ST1::mkeps(r)++List(ST2) else 
    // if(nullable(r)) ST1::mkeps(r)++List(ST2) else  
      
  case CHAR(_) => Nil //for testing mkeps outside lex
  case INIT(r) => mkeps(r) // added to check nullable input
}
 
def mkfin(r: Rexp) : Bits = (r: @unchecked) match {
  case POINT(bs,CHAR(_)) => bs
  case ALT(r1, r2) => 
    if (fin(r1) && fin(r2)) {
      println(s"mkfin1=${mkfin(r1)}, mkfin2=${mkfin(r2)}\ntotalBitsWeight r1 ${totalBitsWeight(mkfin(r1))} and r2 ${totalBitsWeight(mkfin(r2))}")
      if (totalBitsWeight(mkfin(r1))>=totalBitsWeight(mkfin(r2)))
      mkfin(r1) else mkfin(r2)
      
    } else if (fin(r1)) mkfin(r1) else mkfin(r2)
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => (mkfin(r1)) ::: (SE2 :: mkeps(r2) ) // added SE2 for cases of null r2 not having 3/SE2 flag
  case SEQ(r1, r2) => mkfin(r2) 

  case STAR(r) =>  (mkfin(r)) ++ List(ST2)  //ST1:: 
  case INIT(r) => mkfin(r)
}

// shift function from the paper
def shift(m: Boolean, bs: Bits, r: Rexp, c: Char) : Rexp = 
  (r: @unchecked) match {

  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT(bs:+C,CHAR(d)) else CHAR(d)
  
  case POINT(bits,CHAR(d)) => if (m && d == c) POINT(bs:+C,CHAR(d)) else CHAR(d)

  case ALT(r1, r2) => ALT(shift(m, bs:+Z , r1, c), shift(m, bs:+S, r2, c))

  case SEQ(r1, r2) if m && nullable(r1) => SEQ(shift(m, bs:+SE1, r1, c), shift(true,  bs::: (SE1::((mkeps(r1)):+SE2)), r2, c)) 
  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs:+SE1, r1, c), shift(true, ((mkfin(r1))):+SE2, r2, c))
  case SEQ(r1, r2) => SEQ(shift(m, bs:+SE1, r1, c), shift(false, Nil, r2, c)) //Nil

  case STAR(r) if m && fin(r) =>STAR(shift(true, (bs) :+ST1, r, c)) //273-     2732 , 274537
  case STAR(r) if fin(r) => STAR(shift(true, (mkfin(r):+ST1), r, c)) 
  case STAR(r) if m => STAR(shift(m, bs:+ST1, r, c))
  case STAR(r) => STAR(shift(false, Nil, r, c))
}
// the main matching function (by using BINIT only in 
// the first step a mark is shifted into the Rexp)
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

def intern2(r: Rexp) : Rexp = INIT(r)

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
    //case ST1 :: ST2 :: rest => (STARV(Nil), rest) // Empty STAR
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
  case C :: rest =>
    (CHARV('x'), rest) // placeholder value, maybe include empty to simulate the derivatives approach?

  case E :: rest =>
    (EMPTY, rest)

  case Z :: rest => // ALT left
    val (v1, bs1) = bdecode(rest)
    (LEFT(v1), bs1)

  case S :: rest => // ALT right
    val (v2, bs2) = bdecode(rest)
    (RIGHT(v2), bs2)

  case SE1 :: rest => // SEQ start of r1
    val (v1, bs1) = bdecode(rest)
    bs1 match {
      case SE2 :: bs2 => // SEQ start of r2
        val (v2, bs3) = bdecode(bs2)
        (SEQV(v1, v2), bs3)
      case _ => (ERRORVALUE(s"SEQ ERROR, bs=$bs"), bs)
    }

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

  val finReg=matcher2(rexp,s)
  val bits=lex(rexp, s).getOrElse(Nil)
  val markedValue=mDecode(bits, rexp)._1

  println(s"===============\nFinal Reg:\n ${pp(finReg)} \nnullable=${nullable(finReg)}")
  println(s"===============\nMarked bitcode: ${bits}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1
  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"Marked mdecode value == Derivative value :  ${(markedValue == derivValue)}")


  val onlyBitsValue=bdecode(bits)._1
  println(s"===============\nDecoded value without regex, bdecode=${onlyBitsValue}")
  println(s"bdecode compareResults mdecode : ${compareResults(markedValue,onlyBitsValue)}")
}

// testing (("a" | "b") | ("ab") ) | ("bc" | ("c"| "b"))  regex 
@main
def test2() = {
  println("=====Test With SEQ/ALT/CHAR only====")
  val rexp=SEQ(
    ALT(ALT(CHAR('a'),CHAR('b')),SEQ(CHAR('a'),CHAR('b'))) , 
    ALT( SEQ(CHAR('b'),CHAR('c')), ALT(CHAR('c'),CHAR('b'))) ) 
  val s = "abc".toList
  
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
  println(s"===============\nMarked bitcode: ${bits}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1
  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"derivative bitcode == marked bitcode :  ${(markedValue == derivValue)}")


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
  val s = "c".toList
  
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
  println(s"===============\nMarked bitcode: ${bits} and mkfin=${mkfin(finReg)}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1
  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"derivative bitcode == marked bitcode :  ${(markedValue == derivValue)}")


  val onlyBitsValue=bdecode(bits)._1
  println(s"===============\nDecoded value without regex, bdecode=${onlyBitsValue}")
  println(s"bdecode compareResults mdecode : ${compareResults(markedValue,onlyBitsValue)}")
}

// testing ("a"| ONE | ONE) | (("b" | "c") | "c") regex
@main
def test4() = {
  println("=====Test====")
  val rexp= ("a"| ONE | ONE) | (("b" | "c") | "c")
  val s = "c".toList
  
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
  println(s"===============\nMarked bitcode: ${bits}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1
  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"derivative bitcode == marked bitcode :  ${(markedValue == derivValue)}")


  val onlyBitsValue=bdecode(bits)._1
  println(s"===============\nDecoded value without regex, bdecode=${onlyBitsValue}")
  println(s"bdecode compareResults mdecode : ${compareResults(markedValue,onlyBitsValue)}")
}

// testing ("a"| ONE | ONE) | (("b" | "c") | "c") regex
@main
def test5() = {
  println("=====Test====")
  val rexp= ("a"| "ab") | (("c"|"a") ~ ("b" | "c") )
  val s = "ab".toList
  
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
  println(s"===============\nMarked bitcode: ${bits}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1
  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"derivative bitcode == marked bitcode :  ${(markedValue == derivValue)}")


  val onlyBitsValue=bdecode(bits)._1
  println(s"===============\nDecoded value without regex, bdecode=${onlyBitsValue}")
  println(s"bdecode compareResults mdecode : ${compareResults(markedValue,onlyBitsValue)}")
}

// testing (ONE | "a")  | ((ONE ~ ONE) ~ "a") regex
@main
def test6() = {
  println("=====Test====")
  val rexp= (ONE | "a")  | ((ONE ~ ONE) ~ "a")
  val s = "a".toList
  
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
  println(s"===============\nMarked bitcode: ${bits}")
  println(s"Decoded value for Marked=${markedValue}")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  val derivValue=decode( derivBitcode, rexp)._1
  println(s"===============\nDerivatives bitcode: $derivBitcode")
  println(s"Decoded value for derivatives=${derivValue}")

  println(s"===============\ncompareResults = ${compareResults(markedValue, derivValue)}")
  println(s"derivative bitcode == marked bitcode :  ${(markedValue == derivValue)}")


  val onlyBitsValue=bdecode(bits)._1
  println(s"===============\nDecoded value without regex, bdecode=${onlyBitsValue}")
  println(s"bdecode compareResults mdecode : ${compareResults(markedValue,onlyBitsValue)}")
}

// testing STAR("a"|"c") regex
@main
def test7() = {
  println("=====Test====")
  //val rexp= STAR(STAR("c"))
  //ALT(STAR(STAR(CHAR(a))),ALT(ALT(CHAR(c),ONE),ALT(CHAR(a),CHAR(b))))
  val rexp=ALT(ALT(ONE,ALT(CHAR('a'),CHAR('a'))),ALT(SEQ(ONE,CHAR('b')),ALT(CHAR('b'),CHAR('a'))))
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


/* 
def finSize(r: BRexp, nullable:Boolean) : Int = (r: @unchecked) match {
  case BZERO => 0
  case BONE(bs) =>  if(nullable) 1 else 0
  case BCHAR(_,_) => 0
  case BPOINT(r, bs) =>  1 
  case BALT(r1, r2, bs) => 1 + finSize(r1,nullable) + finSize(r2,nullable)
  case BSEQ(r1, r2 , bs) => 1 + finSize(r1,nullable) + finSize(r2,nullable)
  case BSTAR(r, bs) => 1 + finSize(r,nullable)
} */

/*     if(fin(r1) && fin(r2)){
      var mkfinr1=mkfin(r1)
      var mkfinr2=mkfin(r2)

      if(nullable(r1))
      mkfinr1++mkeps(r1)

      if(nullable(r2))
      mkfinr2++mkeps(r2)

        println(s"mkfin r1= ${mkfinr1} and mkfin r2 =${mkfinr2}")
        println(s"nullable r1= ${nullable(r1)} and nullable r2 =${nullable(r2)}")
        if(mkfinr1.length < mkfinr2.length){
          mkfinr1
        }else
          mkfinr2
      }    
    else 
        if (fin(r1)){
        println(s"mkfin fin1 final & nullable=${nullable(r1)}")
        if(nullable(r1)) mkfin(r1)++mkeps(r1) else mkfin(r1) 
        }
        else{
        println(s"mkfin fin2 final & nullable=${nullable(r2)}")
        if(nullable(r2)) mkfin(r2)++mkeps(r2) else mkfin(r2) 
        }  */


         //add code to determine match left most match length 
    /* 
Leftmost Preference: If two matches start at the same position, 
    the one that appears earlier in the ALT should be preferred (leftmost).
Longest Match: If two matches start at the same position, 
    the longer match should be preferred.
 **The Longest Match Rule (or “Maximal Munch Rule”): The longest initial
substring matched by any regular expression is taken as next token.
 **Priority Rule: For a particular longest initial substring, the first (leftmost)
regular expression that can match determines the token.
**Star Rule: A subexpression repeated by∗ shall not match an empty string
unless this is the only match for the repetition.
**Empty String Rule: An empty string shall be considered to be longer than
no match at all.

  val bits1 = mkfin(r1)
  val bits2 = mkfin(r2)
  val (v1, _) = mDecode(bits1, r1)
  val (v2, _) = mDecode(bits2, r2)
  val len1 = matchLength(v1)
  val len2 = matchLength(v2)
  println(s"\nr1 Value: $v1, Length: $len1")
  println(s"\nr2 Value: $v2, Length: $len2")
  if (len1 > len2) mkfin(r1) else mkfin(r2)


*/