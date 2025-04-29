error id: `<none>`.
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_ComplexMarks.sc
empty definition using pc, found symbol in pc: `<none>`.
empty definition using semanticdb
empty definition using fallback
non-local guesses:
	 -rexp/Color.GREEN.
	 -rexp/Color.GREEN#
	 -rexp/Color.GREEN().
	 -rexp/Rexp.Color.GREEN.
	 -rexp/Rexp.Color.GREEN#
	 -rexp/Rexp.Color.GREEN().
	 -rexp/VALUE.Color.GREEN.
	 -rexp/VALUE.Color.GREEN#
	 -rexp/VALUE.Color.GREEN().
	 -derivativesBitcode/Color.GREEN.
	 -derivativesBitcode/Color.GREEN#
	 -derivativesBitcode/Color.GREEN().
	 -MRexp.Color.GREEN.
	 -MRexp.Color.GREEN#
	 -MRexp.Color.GREEN().
	 -Color.GREEN.
	 -Color.GREEN#
	 -Color.GREEN().
	 -scala/Predef.Color.GREEN.
	 -scala/Predef.Color.GREEN#
	 -scala/Predef.Color.GREEN().
offset: 2215
uri: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/play_ComplexMarks.sc
text:
```scala
import $file.rexp, rexp._, rexp.Rexp._, rexp.VALUE._
import $file.derivativesBitcode, derivativesBitcode._

enum Color:
  case GREEN
  case RED
  case PURPLE
  case WHITE

case class Mark(
  marked: Boolean = false,
  bs: List[Int] = List(),
  color: List[Color] = List()
)

enum MRexp {
  case MZERO
  case MONE
  case MCHAR(c: Char, mark: Mark)
  case MALT(r1: MRexp, r2: MRexp)
  case MSEQ(r1: MRexp, r2: MRexp,p1:List[Color],p2:List[Color])
  case MSTAR(r: MRexp)
  case MNTIMES(r: MRexp, n: Int, counter: Int = 0)
  case MINIT(r: MRexp)
}
import MRexp._

def nullable (r: MRexp) : Boolean = r match {
  case MZERO => false
  case MONE => true
  case MCHAR(_,_) => false
  case MALT(r1, r2) => nullable(r1) || nullable(r2)
  case MSEQ(r1, r2,_,_) => nullable(r1) && nullable(r2)
  case MSTAR(_) => true
  case MNTIMES(r, n,counter) => if (n == 0) true else nullable(r)
  case MINIT(r) => nullable(r)
}

def fin(r: MRexp) : Boolean = r match {
  case MZERO => false
  case MONE => false
  case MCHAR(c,mark) => mark.marked 
  case MALT(r1, r2) => fin(r1) || fin(r2)
  case MSEQ(r1, r2,_,_) => (fin(r1) && nullable(r2)) || fin(r2)
  case MSTAR(r) => fin(r)
  case MNTIMES(r, n,counter) => counter == n && fin(r)
  case MINIT(r) => fin(r)
}

def collectPLists(r: MRexp): List[(List[Color], List[Color])] = r match {
  case MSEQ(r1, r2, p1, p2) =>
    (p1, p2) :: (collectPLists(r1) ++ collectPLists(r2))
  case MALT(r1, r2) =>
    collectPLists(r1) ++ collectPLists(r2)
  case MSTAR(r1) =>
    collectPLists(r1)
  case MNTIMES(r1, _, _) =>
    collectPLists(r1)
  case MINIT(r1) =>
    collectPLists(r1)
  case _ => Nil
}

//shift char with position
def shift(m: Boolean, re: MRexp, c: Char, bits:List[Int], pList:List[Color]) : MRexp = {
  re match {
  case MZERO => MZERO
  case MONE => MONE
  case MCHAR(d,mark) => 
    if (m && d == c)
      MCHAR(d, Mark(marked = true, bs = bits , color = pList:::mark.color))
    else 
      MCHAR(d, Mark(marked = false, bs = Nil , color = List()))
      //CHAR(d, Mark())
  case MALT(r1, r2) => MALT(shift(m, r1, c,bits:+0,pList) , shift(m, r2, c,bits:+1,pList) )

  case MSEQ(r1, r2,p1,p2) if m && nullable(r1) =>
    val p1b=List(Color.RED)
    val p1bs=(p1:::p2):::pLis@@t
    val p2b=List(Color.GREEN)
    val p2bs=(p1:::p2):::pList
    MSEQ(shift(m, r1, c, bits:+2,p1b), shift(true, r2, c ,(bits ::: (2:: mkeps(r1))):+3,p2b) ,p1b,p2b)//(B1::bs)::: (SE1::((mkeps(r1)):+SE2))
  case MSEQ(r1, r2,p1,p2) if fin(r1) =>
    val p1b=List(Color.RED)
    val p1bs=(p1:::p2):::pList
    val p2b=List(Color.GREEN)
    val p2bs=(p1:::p2):::pList
    MSEQ(shift(m, r1, c, bits:+2,p1b), shift(true,r2, c,mkfin(r1):+3 ,p2bs),p1b,p2b) 
  case MSEQ(r1, r2,p1,p2) =>
    val p1b=p1:::pList
    val p1bs=pList:+Color.RED
    val p2b=p2:::pList
    val p2bs=pList:+Color.GREEN
    MSEQ(shift(m, r1, c, bits:+2,p1bs), shift(false,r2, c, Nil ,p2bs),p1b,p2b)

  case MSTAR(r) if m && fin(r) => MSTAR(shift(true, r , c , bits ::: (mkfin(r) :+ 4),pList))
  case MSTAR(r) if fin(r) => MSTAR(shift(true,r,c ,mkfin(r) :+ 4,pList)) 
  case MSTAR(r) if m => MSTAR(shift(m, r, c, bits,pList))
  case MSTAR(r) => MSTAR(shift(false, r, c, Nil,pList))

  case MNTIMES(r, n,counter) => 
    if (counter == n) 
      re 
      else{
        if (m || fin(r)) MNTIMES(shift(m || fin(r), r, c,bits,pList), n, counter+1)
        else MNTIMES(shift(false, r, c, bits,pList), n, counter)       
        } 

  case MINIT(r) => shift(true, r, c,bits,pList)  
    } 
}



def mkeps(r: MRexp): List[Int] = (r: @unchecked) match {
  case MZERO => Nil
  case MONE => Nil
  case MCHAR(_, mark) => if (mark.marked) mark.bs else Nil
  case MALT(r1, r2) => if(nullable(r1)) 0 ::mkeps(r1) else 1 :: mkeps(r2)
  case MSEQ(r1, r2,p1,p2) => (mkeps(r1)) ::: ((mkeps(r2)))
  case MSTAR(r) => mkeps(r)++List(1)
  case MNTIMES(_, _, _) =>Nil
  case MINIT(r1) =>mkeps(r1)
}

def mkColor(r: MRexp): Option[Color] = r match {
  case MCHAR(_, mark) if mark.marked =>
    mark.color.headOption
  case MALT(r1, r2) =>
    if (fin(r1)) mkColor(r1) else mkColor(r2)
  case MSEQ(r1, r2, _, _) if fin(r1) && nullable(r) => mkColor(r1)
  case MSEQ(_, r2, _, _) => mkColor(r2)
  case MSTAR(inner)          => mkColor(inner)
  case MNTIMES(inner, _, _)  => mkColor(inner)
  case MINIT(inner)          => mkColor(inner)
  case _                     => None
}

//add boolean argument, if nullable then retrieve from one?
def mkfin(r: MRexp) : List[Int] = (r: @unchecked) match {
  case MCHAR(_, mark) => if (mark.marked) mark.bs else Nil

  case MALT(r1, r2) => 
    if(fin(r1) && fin(r2)){
      val c1=mkColor(r1)
      val c2=mkColor(r2)
     // println(s"mkfin r1= ${mkfin(r1)} and mkfin r2 =${mkfin(r2)}")
     // println(s"mkColor r1= ${mkColor(r1)} and mkColor r2 =${mkColor(r2)}")
      (c1, c2) match {
        case (Some(Color.RED), _) => mkfin(r1)
        case (Some(Color.GREEN), Some(Color.RED)) => mkfin(r2)
        case (Some(Color.GREEN), _) => mkfin(r1)
        case _ => mkfin(r1)
  }
}else if (fin(r1)) mkfin(r1) else mkfin(r2)  
  case MSEQ(r1, r2,p1,p2) if fin(r1) && nullable(r2) => mkfin(r1) ++ mkeps(r2)
  case MSEQ(r1, r2,p1,p2) => mkfin(r2)
  case MSTAR(r) => mkfin(r) ++ List(1)
  case MNTIMES(_,_,_) => Nil
  case MZERO => Nil
  case MONE => Nil
}

def mat(r: MRexp, s: List[Char],pList:List[Color]) : MRexp = s match {
  case Nil => r
  case c::cs => mat(shift(false, r, c,List(),pList), cs,pList)
}

def matcher(r: Rexp, s: List[Char],pList:List[Color]) : Boolean =
  { val reg= intern2(r)
    if (s == Nil) nullable(reg)
     else fin(mat(reg, s,pList))
  }

def lexComplex(r: Rexp, s: List[Char]) : Option[List[Int]] = {
  val reg= intern2(r)
  if matcher(r, s,List())
  then Some( if (s == Nil) mkeps(reg) else mkfin(mat(reg, s,List())))
  else None
}
  
def intern(r: Rexp) : MRexp = (r: @unchecked) match{
  case ZERO            => MZERO
  case ONE             => MONE
  case CHAR(c)         => MCHAR(c, Mark()) 
  case ALT(r1, r2)     => MALT(intern(r1), intern(r2))
  case SEQ(r1, r2)     => MSEQ(intern(r1), intern(r2),List(),List())
  case STAR(r)         => MSTAR(intern(r))
  case NTIMES(r, n)    => MNTIMES(intern(r), n, 0)
  //case INIT(r)         => MINIT(intern(r)) // might remove
}

def intern2(r: Rexp) : MRexp = MINIT(intern(r))

// testing one/emptystring regex 

@main
def test1() = {
  println("\n===== Testing New BitCodes =====\n")
  val rexp=SEQ(ALT(ONE,CHAR('c')) , ALT(SEQ(CHAR('c'),CHAR('c')), CHAR('c')) )
  val s="cc".toList
  val mrexp=intern2(rexp)

  println("=string=")
  println(s)
  
  var pList=List()
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(mrexp, sPart,pList)))
  } 

  val bits=lexComplex(rexp,s).getOrElse(Nil)
  println(s"\n=final list= ${bits}\n")
  //println(s"Decoded value for Marked=${decode( bits, rexp)._1}\n")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  println(s"Derivatives bitcode: $derivBitcode\n")
  //println(s"Decoded value for derivatives=${decode( derivBitcode, rexp)._1}\n")
}

@main
def test2() = {
  val rexp=SEQ(
    ALT(ALT(CHAR('a'),CHAR('b')),SEQ(CHAR('a'),CHAR('b'))) , 
    ALT( SEQ(CHAR('b'),CHAR('c')), ALT(CHAR('c'),CHAR('b'))) ) 
  println(s"regex= $rexp")
  val s = "abc".toList
  val mrexp=intern2(rexp)

  println("=string=")
  println(s)
  
  var pList=List()
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(mrexp, sPart,pList)))
  } 

  val bits=lexComplex(rexp,s).getOrElse(Nil)
  println(s"\n=final list= ${bits}\n")
  //println(s"Decoded value for Marked=${decode( bits, rexp)._1}\n")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  println(s"Derivatives bitcode: $derivBitcode\n")
  //println(s"Decoded value for derivatives=${decode( derivBitcode, rexp)._1}\n")
}

@main
def test3() = {
  val rexp=ALT(ALT(ONE,SEQ(ONE,CHAR('a'))),SEQ(ALT(ONE,CHAR('a')),ALT(ONE,ONE)))
  println(s"regex= $rexp")
  val s = "a".toList
  val mrexp=intern2(rexp)

  println("=string=")
  println(s)
  
  var pList=List()
  for (i <- s.indices) {
  println(s"\n ${i + 1}- =shift ${s(i)}=")
  val sPart = s.take(i + 1)
  println(pp(mat(mrexp, sPart,pList)))
  } 

  val bits=lexComplex(rexp,s).getOrElse(Nil)
  println(s"\n=final list= ${bits}\n")
  //println(s"Decoded value for Marked=${decode( bits, rexp)._1}\n")

  val derivativeR = bders(s, internalize(rexp))
  val derivBitcode = bmkeps(derivativeR)
  println(s"Derivatives bitcode: $derivBitcode\n")
  //println(s"Decoded value for derivatives=${decode( derivBitcode, rexp)._1}\n")
}

def pp(e: MRexp) : String = (e: @unchecked) match {
  case MZERO => "0\n"
  case MONE => "1\n"
  case MCHAR(c,mark) =>  if (mark.marked) s"${"\u001b[32m"} •$c ${mark.bs} {${mark.color}} ${"\u001b[0m"}\n" else s"$c\n"//else if(mark.color==Color.RED) s"${"\u001b[31m"} $c${"\u001b[0m"}\n" 
  case MALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case MSEQ(r1, r2,p1,p2) => s"SEQ {p1=$p1} {p2=$p2}\n" ++ pps(r1, r2)
  case MSTAR(r) => "STAR\n" ++ pps(r)
}
def pps(es: MRexp*) = indent(es.map(pp))

/* 
enum Rexp {
  case ZERO 
  case ONE 
  case CHAR(c: Char, mark: Mark=Mark())
  case ALT(r1: Rexp, r2: Rexp) 
  case SEQ(r1: Rexp, r2: Rexp) 
  case STAR(r: Rexp) 
  case NTIMES(r: Rexp, n: Int , counter: Int = 0)
  case INIT(r:Rexp)
}
import Rexp._ */

/* enum VALUE {
    case EMPTY
    case CHARV(c: Char)  
    case UNMARKED
    case SEQV(v1: VALUE, r2: VALUE )
    case LEFT(v: VALUE)
    case RIGHT(v: VALUE)
    case STARV(vs: List[VALUE])
    case ERRORVALUE
    case EMPTYSTRING
}
import VALUE._ */

/* def decode(r:Rexp , bits:List[Int]): (VALUE,List[Int]) = 
  r match {
    case CHAR(c, mark) =>(CHARV(c), bits ) // to consume bits?
    case ALT(r1, r2) => bits match {
        case 0::bs => 
            val (v,bsp)= decode(r1,bs)
            (LEFT(v),bsp)
        case 1::bs =>
            val (v,bsp)= decode(r2,bs)
            (RIGHT(v),bsp)
    }
    case SEQ(r1, r2) =>
      val (v1, bs1) = decode(r1, bits)
      val (v2, bs2) = decode(r2, bs1)
      (SEQV(v1, v2), bs2) 
    case STAR(r) => bits match {
        case 8 :: bs1 => 
            (STARV(List()), bs1) // terminate recursion for STAR

        case 7 :: bs1 =>   
            val (v, bs2) = decode(r, bs1)
            val (STARV(vs), bsv) = decode(STAR(r), bs2) 
            (STARV(v :: vs), bsv) 
        case _ => 
        (STARV(List()), bits)
    
    }// end of match r   
    case ONE => (EMPTY, bits) 
    
} */

/* 
// some syntax sugar for regexes
def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
// strings are coerced into Rexps
given Conversion[String, Rexp] = s => charlist2rexp(s.toList)

//val ABCD : Rexp = "abcd"

extension (r: Rexp) {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

// pretty-printing REGs
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

def pp(e: Rexp) : String = e match {
  case ZERO => "0\n"
  case ONE => "1\n"
  case CHAR(c,mark) => if(mark.marked) s"•$c bs={${mark.bs}} color={${mark.color}}\n" else s"$c bs={${mark.bs}} color={${mark.color}}\n"
  case ALT(r1, r2) => s"ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => s"SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
  case NTIMES(r, n,counter) => 
    s"NTIMES{$n}{fin r=${fin(r)}, null r=${nullable(r)} counter=$counter}\n" ++ pps(r)
    s"• NTIMES {$n}{fin r=${fin(r)}, null r=${nullable(r)}, counter=$counter}\n" ++ pps(r) 
  case INIT(r) => s"INIT\n" ++ pps(r)   
}
def pps(es: Rexp*) = indent(es.map(pp))
 */
```


#### Short summary: 

empty definition using pc, found symbol in pc: `<none>`.