
// not working 
import scala.language.implicitConversions
import $file.rexp, rexp._
import $file.enumerate, enumerate._
import $file.regenerate//, regenerate._
import $file.rebit

def decode_aux(r: Rexp, bs: Bits) : (Val, Bits) = (r, bs) match {
  case (ONE, bs) => (Empty, bs)
  case (CHAR(c), bs) => (Chr(c), bs)
  case (ALT(r1, r2), Lf::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    (Left(v), bs1)
  }
  case (ALT(r1, r2), Ri::bs) => {
    val (v, bs1) = decode_aux(r2, bs)
    (Right(v), bs1)
  }
  case (SEQ(r1, r2), bs) => {
    val (v1, bs1) = decode_aux(r1, bs)
    val (v2, bs2) = decode_aux(r2, bs1)
    (Sequ(v1, v2), bs2)
  }
  case (STAR(_), En::bs) => (Stars(Nil), bs)
  case (STAR(r1), Nx::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Stars(vs), bs2) = (decode_aux(STAR(r1), bs1)  : @unchecked)
    (Stars(v::vs), bs2)
  }
  /*case (STAR(r1), bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Stars(vs), bs2) = (decode_aux(STAR(r1), bs1)  : @unchecked)
    (Stars(v::vs), bs2)
  }*/
}

def dec2(r: Rexp, bs: Bits) = decode_aux(r, bs) match {
  case (v, Nil) => v
  case _ => throw new Exception("Not decodable")
}


def hasNestedMStar(r: Rexp): Boolean = {
    def containsMStar(r: Rexp): Boolean = r match {
      case STAR(_) => true
      case ALT(r1, r2) => containsMStar(r1) || containsMStar(r2)
      case SEQ(r1, r2) => containsMStar(r1) || containsMStar(r2)
      case _ => false
    }
    r match {
      case STAR(inner) => 
        if (containsMStar(inner)) true
        else hasNestedMStar(inner)
      case ALT(r1, r2) => hasNestedMStar(r1) || hasNestedMStar(r2)
      case SEQ(r1, r2) => hasNestedMStar(r1) || hasNestedMStar(r2)
      case _ => false
    }
}

def isStar(r: Rexp) : Boolean = r match {
    case STAR(_) => true
    case _ => false
} 

  
def mat(r: Rexp, s: List[Char]) : Rexp = s match {
    case Nil => r
    case c::cs => cs.foldLeft(shift(true, Nil, r, c))((r, c) => shift(false, Nil, r, c))
}
  
def matcher(r: Rexp, s: List[Char]) : Boolean =
    if (s == Nil) nullable(r) else fin(mat(r, s))
  
def lex(r: Rexp, s: List[Char]) : Option[Set[Bits]] = {
    if matcher(r, s)
    then Some(if (s == Nil) Set(mkeps(r)) else mkfin2(mat(r, s)))
    else None
}
  
def lexer(r: Rexp, s: List[Char]) : Option[Set[Val]] = {
    lex(r, s).map(_.map(dec2(r, _)))
}

def mkeps(r: Rexp) : Bits = r match {
  case ONE => Nil
  case POINT(bs, CHAR(_)) => bs
  case POINT(bs, ONE) => List(En)
  case ALT(r1, r2) => if (nullable(r1)) Lf:: mkeps(r1) else Ri:: mkeps(r2) 
  case SEQ(r1, r2) => mkeps(r1) ++ mkeps(r2)
  case STAR(r) => List(En)
  case NTIMES(r,n) => List(En)
}

def fin(r: Rexp) : Boolean = (r: @unchecked) match {
  case ZERO => false
  case ONE => false
  case POINT(_, ONE) => false
  case CHAR(_) => false
  case POINT(_, CHAR(_)) => true
  case ALT(r1, r2) => fin(r1) || fin(r2)
  case SEQ(r1, r2) => (fin(r1) && nullable(r2)) || fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r,n) => if(n==0) fin(r) else false
}

def mkfin(r: Rexp) : Bits = r match {
  case POINT(bs, CHAR(_)) => bs
  case ALT(r1, r2) => if (fin(r1)) mkfin(r1) else mkfin(r2) 
  case SEQ(r1, r2) if fin(r1) && nullable(r2) => mkfin(r1) ++ mkeps(r2)
  case SEQ(r1, r2) if fin(r2) =>  mkfin(r2)
  case STAR(r) => mkfin(r) ++ List(En)
  case NTIMES(r,n) => mkfin(r) ++ List(En)
} 

def mkfin2(r: Rexp) : Set[Bits] = r match {
  case POINT(bs, CHAR(_)) => Set(bs)
  case ALT(r1, r2) if fin(r1) && fin(r2) => mkfin2(r1) | mkfin2(r2)
  case ALT(r1, r2) if fin(r1) => mkfin2(r1)
  case ALT(r1, r2) if fin(r2) => mkfin2(r2) 

  //case SEQ(r1, r2) if ((fin(r1) && nullable(r2)) && fin(r2)) => mkfin2(r1).map(_ ++ mkeps(r2))  | mkfin2(r2)
  case SEQ(r1, r2) if ((fin(r1) && nullable(r2))) => mkfin2(r1).map(_ ++ mkeps(r2)) 
  case SEQ(r1, r2) => mkfin2(r2)
  case STAR(r) => mkfin2(r).map(_ ++ List(En))
  case NTIMES(r,n) =>mkfin2(r).map(_ ++ List(En))
}

def shift(m: Boolean, bs: Bits, r: Rexp, c: Char) : Rexp = (r: @unchecked) match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(d) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case POINT(_, CHAR(d)) => if (m && d == c) POINT(bs, CHAR(d)) else CHAR(d)
  case POINT(bs, ONE) => ONE

  case ALT(r1, r2) => ALT(shift(m, bs:+Lf, r1, c), shift(m, bs:+Ri, r2, c))

  case SEQ(r1, r2) if m && nullable(r1) => 
    ALT( SEQ(shift(true, bs, r1, c), shift(fin(r), bs++mkeps(r1) , r2, c) ), shift(true,bs++mkeps(r1), r2, c) )

    //SEQ(shift(m, bs, r1, c), shift(true, bs ::: mkeps(r1), r2, c))

    

  case SEQ(r1, r2) if fin(r1) => SEQ(shift(m, bs, r1, c), shift(true, mkfin(r1), r2, c))
  case SEQ(r1, r2) => SEQ(shift(m, bs, r1, c), shift(false, Nil, r2, c))

  //case STAR(r) if fin(r) => SEQ(shift(true, mkfin(r):+Nx, r, c),STAR(r))
  case STAR(r) if m => shift(true, bs:+Nx ,SEQ(r,STAR(r) ),c)
  case STAR(r) => STAR(r)  // STAR(shift(false, Nil, r, c))
  
  case NTIMES(r,n) if n<=0 => POINT(List(En),ONE)
  case NTIMES(r,n) if m =>SEQ(shift(true, bs:+Nx, r, c) , NTIMES(r,n-1)) 
  case NTIMES(r,n) => NTIMES(shift(false, Nil, r, c),n)
}

def OPT(r: Rexp) = ALT(r,ONE)

// testing new implementation
@main
def basic() = {
  println("=====Test====")
  //val br2=NTIMES("a",5) ~ ("aa"|"a") 
  val br2= %( "a" | "aa" )
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

//%("a")~(%("ab") ~ %("b")) - Works now - after r.r* and SEQ nullable = ALT
@main
def test1() = {
  println("=====Test====")
  val br2 = %("a")~(%("ab") ~ %("b"))
  val s = "ab".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

//%("a") ~ ("aa"|"a") -  works now - check more input chars
@main
def test2() = {
  println("=====Test====")

  val br2= %("a") ~ ("aa"|"a")
  val s = "aaaaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

//(ONE  |  %("c"|"d")) - works now - check more input chars
@main
def test3() =  {
  println("=====Test====")
  val br2 = (ONE  |  %("c"|"d"))
  //val br2= STAR( STAR("a") )
  val s = "ccc".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

// ONE  |  "a" ~ %("a") - Works now - after r.r* and SEQ nullable = ALT
@main
def test4() =  {
  println("=====Test====")
  val br2 = ONE  |  "a" ~ %("a")
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

// ONE | %("a" | "aa") - fails on aaa - after r.r* and SEQ nullable = ALT
@main
def test5() =  {
  println("=====Test====")
  val br2 = ONE |  %("a" | "aa") 
  
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

//Nested STAR
//ONE|%(%("a")) - works now - check more input chars
@main
def test6() = {
  println("=====Test====")
  val br2= %(%("a"))
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

//ONE | %(%("a")) - works now - check more input chars
@main
def test7() = {
  println("=====Test====")
  val br2= ONE | %(%("a"))
  val s = "aa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}


//%("aa") ~ %(%("a")) - doesn't work in this version
@main
def test8() = {
  println("=====Test====")
  val br2= %("aa") ~ %(%("a"))
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

//( %("a") ~ %("a") ) ~ "a" - works now - check more input chars
@main
def test9() = {
  println("=====Test====")
  val br2= ( %("a") ~ %("a") ) ~ "a"
  //val br2=ALT(ONE,STAR(ALT(CHAR('a'),SEQ(CHAR('a'),CHAR('a')))))
  val s = "aaa".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}

// empty
@main
def test10() = {
  println("=====Test====")
  val br2= ONE 
  val s = "".toList
  println(s"Regex:\n${pp(br2)}\n")
  println("=string=")
  println(s)

  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  println(pp(mat(br2, s.take(i + 1))))
  } 

  println(s"=final list=")
  println(lex(br2, s))
  println(s"=reference list=") 
  println(rebit.lex(br2, s))
}


import scala.util._

@main
def weakTest() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (1, cs => STAR(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1)))
      )
  val alphabet = LazyList('a', 'b')
  var i=BigInt(0)
  val numRegexes=BigInt(10_000_000_000L)
  while(i<= numRegexes){
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "")
      { val v1s = Try(lex(r, s.toList)).getOrElse(None)
        val v2 = rebit.lex(r, s.toList)
        if (v1s.isDefined && !v1s.get.contains(v2)) {
        
          println(s"[${i}]- reg: $r str: $s")
          println(s"mark: ${v1s.get} bder: $v2")
          println(s"mark: ${lex(r, s.toList).get} bder: ${rebit.lex(r, s.toList)}")

          //get input to continue or stop
          print("Continue testing? (y/n): ")
          val input = scala.io.StdIn.readLine().trim.toLowerCase
          
          if (input != "y") {
            println("End test.")
            System.exit(0)
          }
          
        }
      }
      i+=1
  }//end whild
  println("\nAll tests passed!")
}


// pretty-printing Rexps

def implode(ss: Seq[String]) = ss.mkString("\n")
def explode(s: String) = s.split("\n").toList

def lst(s: String) : String = explode(s) match {
  case hd :: tl => implode(" └" ++ hd :: tl.map("  " ++ _))
  case Nil => ""
}

def mid(s: String) : String = explode(s) match {
  case hd :: tl => implode(" ├" ++ hd :: tl.map(" │" ++ _))
  case Nil => ""
}

def indent(ss: Seq[String]) : String = ss match {
  case init :+ last => implode(init.map(mid) :+ lst(last))
  case _ => "" 
}

//X-1 deleted - adjusted for test, X-1
def pp(e: Rexp) : String = (e: @unchecked) match {
  case ZERO => "0\n"
  case ONE => "1\n"
  case CHAR(c) => s"$c\n"
  case POINT(bs, CHAR(c)) => s"•$c:${bs.mkString(",")}\n" 
  case POINT(bs, ONE) => s"1:${bs.mkString(",")}\n"
  //case POINT(bs, STAR(r))=> s"•STAR bs=${bs.mkString(",")}\n" ++ pps(r)
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
  case NTIMES(r, n) => s"NTIMES:$n\n" ++ pps(r) // new to testX1.
}
def pps(es: Rexp*) = indent(es.map(pp))


/*
@main
def weakTest() = {
  given rexp_cdata : CDATA[Rexp] = List(
        (0, _ => ONE),
        (0, _ => ZERO),
        (0, _ => CHAR('a')),
        (0, _ => CHAR('b')),
        (0, _ => CHAR('c')),
        (1, cs => STAR(cs(0))),
        (2, cs => ALT(cs(0), cs(1))),
        (2, cs => SEQ(cs(0), cs(1)))
      )

  val alphabet = LazyList('a', 'b')
  for (i <- (0L to 10_000_000L)) {
    val r = enumerate.decode(i)
    if (i % 100_000 == 0) { print("*") }
    for (s <- (regenerate.generate_up_to(alphabet)(10)(r).take(9)) if s != "")
      { val v1s = Try(lexer(r, s.toList)).getOrElse(None)
        val v2 = rebit.blexer(r, s)
        if (v1s.isDefined && !v1s.get.contains(v2)) {
          println(s"[$i]reg: $r str: $s")
          println(s"mark: ${v1s.get} bder: $v2")
          println(s"mark: ${lex(r, s.toList).get} bder: ${rebit.lex(r, s.toList)}")
          System.exit(1)
        }
      }
  }
}
*/

//shift
/*    case SEQ(r1, r2) if m && isStar(r2) => 
    if(fin(r1))
    SEQ(shift(m, bs, r1, c), shift(true,mkfin(r1),r2,c))
    else
    SEQ(shift(m, bs, r1, c), shift(true,bs:+Nx,r2,c)) 
*/
//case STAR(r) if m && fin(r)=> STAR(SEQ(shift(true, (bs:::mkfin(r)):+Nx, r, c),STAR(r)) )
//case STAR(r) if fin(r)=> STAR(SEQ(shift(true, (mkfin(r)):+Nx, r, c), STAR(r)))
//case SEQ(r1, r2) if m && nullable(r1) => SEQ(shift(m, bs, r1, c), shift(true, bs:::mkeps(r1) , r2, c))
//val r2s= if fin(r1) then shift(true,mkfin(r1), r2, c) else shift(true,bs:::mkeps(r1), r2, c)
//case NTIMES(r,n) if fin(r) =>SEQ(shift(true, (mkfin(r):+En),r, c) , NTIMES(r,n-1)) 

//mkfin2
//case SEQ(r1, r2) if ((fin(r1) && isStar(r2)))=>mkfin2(r1).map(_ ++ List(En))
//case SEQ(r1, r2) if ((fin(r1) && nullable(r2)) && fin(r2)) => mkfin2(r1).map(_ ++ mkeps(r2))  | mkfin2(r2)
//mkfin
//case SEQ(r1, r2) if ((fin(r1) && isStar(r2)))=>mkfin(r1) ++ List(En)
