
package ammonite
package $file
import _root_.ammonite.interp.api.InterpBridge.{
  value => interp
}
import _root_.ammonite.interp.api.InterpBridge.value.{
  exit,
  scalaVersion
}
import _root_.ammonite.interp.api.IvyConstructor.{
  ArtifactIdExt,
  GroupIdExt
}
import _root_.ammonite.compiler.CompilerExtensions.{
  CompilerInterpAPIExtensions,
  CompilerReplAPIExtensions
}
import _root_.ammonite.runtime.tools.{
  browse,
  grep,
  time,
  tail
}
import _root_.ammonite.compiler.tools.{
  desugar,
  source
}
import _root_.mainargs.{
  arg,
  main
}
import _root_.ammonite.repl.tools.Util.{
  PathRead
}
import _root_.ammonite.repl.ReplBridge.value.{
  codeColorsImplicit
}


object `marked-BitcodeTest`{
/*<script>*/import scala.compiletime.ops.string

enum Rexp {
  case ZERO
  case ONE
  case CHAR(c: Char ,marked:Boolean =false) //marked: Int = 0
  case ALT(r1: Rexp, r2: Rexp)
  case SEQ(r1: Rexp, r2: Rexp)
  case STAR(r: Rexp)
  case NTIMES(r: Rexp, n: Int, nmark:List[Int] = List() , counter:Int=0) 
  
  override def toString: String = this match {
    case ZERO => "ZERO"
    case ONE => s"ONE"
    case CHAR(c, marked) => s"CHAR($c , $marked)"
    case ALT(r1, r2) => s"ALT($r1, $r2)"
    case SEQ(r1, r2) => s"SEQ($r1, $r2)"
    case STAR(r) => s"STAR($r)"
    case NTIMES(r, n, nmark ,counter) => s"NTIMES($r, n=$n, nmark=$nmark , count=$counter)"
  }
}
import Rexp._

enum Rexpb {
  case ZEROb
  case ONEb(bs: List[Int] = List())
  case CHARb(c: Char ,marked:Boolean =false ,bs: List[Int] = List()) //marked: Int = 0
  case ALTb(r1: Rexpb, r2: Rexpb , bs: List[Int] = List())
  case SEQb(r1: Rexpb, r2: Rexpb , bs: List[Int] = List())
  case STARb(r: Rexpb ,   bs: List[Int] = List())
  case NTIMESb(r: Rexpb, n: Int,nmark:List[Int] = List(0) , counter:Int=0 , bs: List[Int] = List()) 
  case INITb(r: Rexpb , bs: List[Int] = List())
  
  override def toString: String = this match {
    case ZEROb => "ZEROb"
    case ONEb(bs) => s"ONEb bs:$bs"
    case CHARb(c, marked ,bs) => s"CHARb($c , $marked $bs)"
    case ALTb(r1, r2,bs) => s"ALTb($r1, $r2) bs:$bs"
    case SEQb(r1, r2,bs) => s"SEQb($r1, $r2) bs:$bs"
    case STARb(r,bs) => s"STARb($r) bs:$bs"
    case NTIMESb(r, n, nmark ,counter,bs) => s"NTIMESb($r, n=$n, nmark=$nmark , count=$counter , bs:$bs)"
    case INITb(r,bs) => s"INITb($r) bs:$bs"
  }
}
import Rexpb._


def OPT(r: Rexp): Rexp = ALT(r, ONE)

def shift(mark: Boolean,re: Rexpb, c: Char ): Rexpb = re match {
    case ZEROb => ZEROb
    case ONEb(bs)=> ONEb(bs)

    case CHARb(ch,marked,bs) => {
        if(mark && ch == c)
        CHARb(ch,mark && ch == c,bs) 
        else CHARb(ch,mark && ch == c,bs) // maybe [] for bs ?
    }
    case ALTb(r1, r2,bs) => ALTb(shift(mark,r1,c),shift(mark,r2,c),bs) 
    case SEQb(r1,r2,bs) =>
        //bs [](der c r1· r2) + fuse (mkeps r1) (der c r2) if nullable(r1)
        //ALT(SEQ(der(c, r1), r2), der(c, r2 ))
        SEQb(shift(mark, r1, c), shift((mark && nullable(r1)) || fin(r1), r2, c),bs)
      // if (mark && nullable(r1))
      // {
      //   ALTb(
      //       SEQb(shift(mark, r1, c), shift(fin(r1), r2, c)) // [] as bs for the SEQ
      //           , 
      //       fuse(mkeps_marked(r1) ,shift(true, r2, c)) , bs)
      // }
      // else{
      //   SEQb(shift(mark, r1, c), shift(fin(r1), r2, c) , bs)   
      // }  
   

    case STARb(r,bs) => STARb(shift(mark || fin(r), r,c),bs ++mkeps_marked2(r)++ List(0)) // could be if fin(r) then list(0) else list(1)
    
    case NTIMESb(r, n,nmark,counter,bs) => // maybe shift false if n==0 or counter == n?
      if (n==0){
        ONEb(bs)
      }else{
        if(counter == n){
           val test=shift(mark || fin(r), r, c)
           val v= if(fin(test)) 1 else 0
           NTIMESb(test, n,v ::nmark,counter,bs) // shifting without adding to counter 
        } else 
            if (mark || fin(r))
            { 
              val v= if(fin(r)) 1 else 0
              NTIMESb(shift(mark || fin(r), r, c), n , v::nmark,counter+1,bs)
            }
            else {
                val v= if(fin(r)) 1 else 0
                NTIMESb(shift(false, r, c), n,v::nmark,counter,bs)
            }
      }  
    case INITb(r,bs) => shift(true, r, c)
     
}

def nullable(r: Rexpb) : Boolean = r match {
  case ZEROb => false
  case ONEb(bs) => true
  case CHARb(_,_,_) => false
  case ALTb(r1, r2,bs) => nullable(r1) || nullable(r2)
  case SEQb(r1, r2,bs) => nullable(r1) && nullable(r2)
  case STARb(_,_) => true
  case NTIMESb(r, n ,nmark , counter ,bs) => if (n == 0) true else nullable(r) //?
  case INITb(r,bs) => nullable(r)
}

def fin(r: Rexpb) : Boolean = r match {
  case ZEROb => false
  case ONEb(bs) => false
  case CHARb(c,marked,bs) => marked
  case ALTb(r1, r2,bs) => fin(r1) || fin(r2)
  case SEQb(r1, r2,bs) => (fin(r1) && nullable(r2)) || fin(r2) 
  case STARb(r,bs) => fin(r)
  case NTIMESb(r,n,nmark , counter,bs) => 
    if(n == counter || (counter<n && nullable(r))) 
    fin(r)  else false // ? && nmark.sum <= n+1

}

def mat(r: Rexpb, s: List[Char]) : Rexpb = s match {
  case Nil => r
  case c::cs => mat(shift(false, r, c), cs)
}

def matcher(r: Rexpb, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))


def matcher2(r: Rexpb, s: List[Char]) : Rexpb =
  val x=mat(r, s)
  x match {
    case INITb(r,bs) => 
      //println(s"Final Reg=$r")
      r
    case _ => 
     // println(s"Final Reg=$x")
      x
  }
  
enum VALUE {
  case ZEROV
  case ONEV
  case CHARV(c: Char)
  case UNMARKED(s:String)
  case SEQV(v1: VALUE, r2: VALUE )
  case LEFT(v: VALUE)
  case RIGHT(v: VALUE)
  case STARV(vs: List[VALUE])
}
import VALUE._

def mkeps_marked(r: Rexpb): List[Int] = r match {
    case ONEb (bs) => bs // ???
    case CHARb(c,marked,bs) => if(marked) bs else List() //???
    case ALTb(r1, r2,bs) =>
        //bs @ (mkeps r1) if nullable(r1)
        if (nullable(r1))  (bs ++ mkeps_marked(r1))
        //bs @ (mkeps r2)
        else (bs ++ mkeps_marked(r2))
    case SEQb(r1, r2,bs) => (bs ++ mkeps_marked(r1) ++ mkeps_marked(r2))
    case STARb(r1,bs) => (bs ++ List(1))
    case NTIMESb(r, n, nmark,counter,bs) =>
      if (n == 0) mkeps_marked(ONEb(bs))
      else if (counter == n) (bs ++ List(1)) //???
      else bs // ????? 
  }

def mkeps_marked2(r: Rexpb): List[Int] = r match {
    case ONEb(bs) => bs// 
    case CHARb(_, marked, bs) => if (marked) bs else List() 
    case ALTb(r1, r2, bs) =>
        if (fin(r1))  bs ++ mkeps_marked2(r1) 
        else if (fin(r2)) bs ++ mkeps_marked2(r2) 
        else List() //bs
    case SEQb(r1, r2, bs) =>
          if (fin(r1) && nullable(r2)) bs ++ mkeps_marked2(r1) ++ mkeps_marked2(r2) 
          else if (fin(r2)) bs ++  mkeps_marked2(r2)
          else List() //bs
    case STARb(r, bs) =>
        if (fin(r)) bs ++ List(1) 
        else List() 

    case NTIMESb(r, n, nmark, counter, bs) =>
        if (counter == n && fin(r)) bs ++ nmark ++ mkeps_marked2(r) 
        else List() //bs
    case INITb(r, bs) => bs ++ mkeps_marked2(r)
    case ZEROb => List() 
}


 def fuse(cs: List[Int], r: Rexpb): Rexpb = r match {
    case ZEROb => ZEROb
    case ONEb(bs) => ONEb(cs ++ bs)
    case CHARb(c, marked, bs) => CHARb(c, marked, cs ++ bs)
    case ALTb(r1, r2, bs) => ALTb(r1, r2, cs ++ bs)
    case SEQb(r1, r2, bs) => SEQb(r1, r2, cs ++ bs)
    case STARb(r, bs) => STARb(r, cs ++ bs)
    case NTIMESb(r, n, nmark, counter, bs) => NTIMESb(r, n, nmark, counter, cs ++ bs)
    case INITb(r, bs) => INITb(r, cs ++ bs)
}



def decode(r: Rexp, bs: List[Int]): (VALUE, List[Int]) = r match {
  case ONE => (ONEV, bs) // not sure this should be included
  case CHAR(c, _) => (CHARV(c), bs) // (2) decode (c) bs = (Char(c), bs)
  case ALT(r1, r2) => bs match {
    case 0 :: bs1 => // (3) decode (r1 + r2) 0 :: bs =  (Left(v), bs')  where decode r1 bs => v,bs' 
    val (v, bsp) = decode(r1, bs1)
    (LEFT(v), bsp) 
    case 1 :: bs1 => // (4) decode (r1 + r2) 1 :: bs = (Right(v), bs') where decode r2 bs => v,bs'
    val (v, bsp) = decode(r2, bs1)
    (RIGHT(v), bsp)
    case x =>
    (ZEROV, bs) // in case of something else, may need to remove it but just incase
  }
  case SEQ(r1, r2) =>  // (5) decode (r1 · r2) bs = (Seq(v1, v2), bs3) where decode r1 bs => v1,bs2 and decode r2 bs2 =>v2,bs3 
    val (v1, bs2) = decode(r1, bs)
    val (v2, bs3) = decode(r2, bs2)
    (SEQV(v1, v2), bs3) 

  case STAR(r) => bs match {
    case 1 :: bs1 => 
    (STARV(List()), bs1) // terminate recursion for STAR
    case 0 :: bs1 =>   
    val (v, bs2) = decode(r, bs1)
    val (STARV(vs), bsv) = decode(STAR(r), bs2) 
    (STARV(v :: vs), bsv) 
    case _ => 
    (STARV(List()), bs) // Edge case: No matches in STAR
  }// end of match r
}

def intern2(r: Rexp) : Rexpb = INITb(intern(r),List())

def intern(r: Rexp) : Rexpb = r match{
  case ZERO => ZEROb
  case ONE => ONEb(List())
  case CHAR(c,marked) => CHARb(c,marked,List())
  case ALT(r1, r2) => 
    ALTb(fuse(List(0), intern(r1)), fuse(List(1), intern(r2)), List())
  case SEQ(r1, r2) => SEQb(intern(r1),intern(r2),List())
  case STAR(r) => STARb(intern(r),List())
  case NTIMES(r, n, nmark,counter) => NTIMESb(intern(r),n,nmark,counter, List())
}

@main
def test01() = {
  val rexp=STAR( ALT( ALT(CHAR('a'),CHAR('b')) , CHAR('c') ) )
  val rexp2 = intern2(rexp)

    println("=============== Test ===============")
    val s="abc".toList
    println(s"String: $s\n")
    val finReg=matcher2(rexp2, s)
    println(s"Original size=${size(rexp2)} Result= ${fin(finReg)} \n")

    for (i <- s.indices) {
    println(s"${i + 1}- =shift ${s(i)}=")
    val sPart = s.take(i + 1)
    println(pp(mat(rexp2, sPart)))
    }

    println("\n=============== Final Reg ===============n")
    println(s"Size=${size(finReg)} , Tree= \n ${pp(finReg)}\n")
    println("\n=============== bitcodes ===============n")

    val mkepsValue = mkeps_marked2(finReg)
    println(s"mkeps= $mkepsValue")
    val decodeValue=decode(rexp,mkepsValue)
    println(s"decode=$decodeValue")  
}

val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

@main
def test02() = {
  // for (i <- 0 to 7000000 by 500000) {
 // }
 //:+ 'b'
  val evil2b=intern2(EVIL2)
  val i=1000
  println(f"$i: ${time_needed(2, matcher(evil2b, ("a" * i).toList))}%.5f")
}

//testing unshift
@main
def test03() = {
  val a=CHAR('a')
  val b=CHAR('b')
  val rexp=intern(SEQ(a , b))

  println(s"Original Reg:$rexp \n")
  val shiftedOnce=shift(true,rexp,'a')
  val shiftedTwice=shift(true,shiftedOnce,'b')
  println(s"Shifted Once with a: ${pp(shiftedOnce)}\n")
  println(s"Shifted twice: ${pp(shiftedTwice)}\n")

  val unshiftedOnce=unshift(true,shiftedTwice,'b')
  println(s"Unshifted Once with b: ${pp(unshiftedOnce)}\n")

  println("\n========================\n")
  val rexp2=intern(STAR(a))
  val shiftedOnce2=shift(true,rexp2,'a')
  println(s"Shifted Once: ${pp(shiftedOnce2)}\n")

  val unshiftedOnce2=unshift(true,shiftedOnce2,'a')
  println(s"Unshifted Once: ${pp(unshiftedOnce2)}\n")

}

def size(r: Rexpb) : Int = r match {
  case ZEROb => 1
  case ONEb(bs) => 1
  case CHARb(_,_,_) => 1
  case ALTb(r1, r2,bs) => 1 + size(r1) + size(r2)
  case SEQb(r1, r2,bs) => 1 + size(r1) + size(r2)
  case STARb(r,bs) => 1 + size(r)
  case NTIMESb(r,n, nmark,counter,bs) => 1 + size(r) 
  case INITb(r,bs) => 1 + size(r)   
}

def SeqNTIMES(r: Rexp): Rexp = r match {
  case ZERO => ZERO
  case ONE  => ONE
  case CHAR(c, marked) => CHAR(c, marked)
  case ALT(r1, r2) =>
    ALT(SeqNTIMES(r1), SeqNTIMES(r2))
  case SEQ(r1, r2) =>
    SEQ(SeqNTIMES(r1), SeqNTIMES(r2))
  case STAR(r1) =>
    STAR(SeqNTIMES(r1))
  case NTIMES(r, n, nmark,counter) =>
    val expandR = SeqNTIMES(r)
    if (n <= 0) ONE else
        if (n == 1) expandR else 
            { SeqNTIMES( SEQ( expandR, NTIMES(expandR,n-1,nmark,counter) ) ) }
}

def EVIL1(n: Int) = SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))
// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

//@arg(doc = "Test (a?{n}) (a{n})")
@main
def test1() = {
  for (i <- 0 to 11000 by 1000) {
    println(f"$i: ${time_needed(2, matcher(intern2(EVIL1(i)), ("a" * i).toList))}%.5f Result: ${matcher(intern2(EVIL1(i)), ("a" * i).toList)} ")
  }
}

//@arg(doc = "Test (a*)* b")
@main
def test2() = {
  for (i <- 0 to 10000000 by 500000) {
    println(f"$i: ${time_needed(2, matcher(intern2(EVIL2), ("a" * i).toList))}%.5f")
  }
} 

//@arg(doc = "All tests.")
def all() = { test1(); test2() } 

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

def pp(e: Rexpb) : String = e match {
  case ZEROb => "0\n"
  case ONEb(bs) => s"1 bs: $bs\n"
  case CHARb(c,marked,bs) => if (marked) s"•$c bs: $bs \n" else s"$c bs: $bs\n"
  case ALTb(r1, r2,bs) => s"ALT bs: $bs \n" ++ pps(r1, r2)
  case SEQb(r1, r2,bs) => s"SEQ bs: $bs \n" ++ pps(r1, r2)
  case STARb(r,bs) => s"STAR bs: $bs \n" ++ pps(r) //if(n == 0 ) fin(r) 
  case NTIMESb(r,n,nmark,counter,bs) => if (n == counter && fin(r)) s"•NTIMES(n=$n) bs: $bs nmark: $nmark counter=$counter\n" ++ pps(r) 
    else s"NTIMES(n=$n) bs: $bs nmark: $nmark counter=$counter\n" ++ pps(r)
  case INITb(r,bs) => s"INIT bs: $bs\n" ++ pps(r)
}
def pps(es: Rexpb*) = indent(es.map(pp))

// maybe we dont'need mark in unshift
def unshift(mark: Boolean, r: Rexpb, c: Char): Rexpb = r match {
  case ZEROb => ZEROb
  case ONEb(bs) => ONEb(bs)
  case CHARb(ch,marked,bs) => 
    //  m && (d == c) from shift
    //revers ? (m || d != c, d)
    val newMark = if (ch == c) !marked else false
    CHARb(ch, newMark ,bs) 
  case ALTb(r1, r2,bs) => ALTb(unshift(mark, r1, c), unshift(mark, r2, c) , bs)
  case SEQb(r1, r2,bs) =>
      SEQb(unshift(mark, r1, c), unshift(fin(r1), r2, c) ,bs)
  case STARb(r,bs) =>
    STARb(unshift(mark, r, c),bs) // STAR(unshift(mark * fin(r), r, c))
  
  case NTIMESb(r, n,nmark,counter,bs) =>
    if (n == 0) NTIMESb(r, n,nmark,counter,bs) else {
      if (mark || fin(r)) {
        val v = if(mark && fin(r)) 1 else 0
        NTIMESb(unshift(mark && fin(r), r, c), n, v::nmark,counter-1 , bs)
      } else {
        NTIMESb(unshift(false, r, c), n, 0::nmark,counter , bs)
      }
    }
  case INITb(r,bs) => INITb(unshift(mark, r, c),bs)
}





/*
def matcher(r: Rexp , s: String) : Int = s.toList match {
  case Nil => nullable(r)
//case c :: cs => fin(cs.foldLeft(shift(true, r, c)) { (acc, c) =>
//  shift(false, acc, c) })
  case c :: cs => //fin(cs.foldLeft(shift(1, r, c))(shift(0, _, _)))
    val x=cs.foldLeft(shift(1, r, c))(shift(0, _, _))
    println(s"\n Last Reg: \n $x")
    fin(x)
}
*/



/*
mkeps_marked2
    case bs1::Nil =>
        println(s"decode r1: $r1  and bs1= $bs1") 
        if(bs1 == 0){
            val (v, bsp)= decode(r1, bs1::Nil)
            (LEFT(v), bsp)
        }
        else {
            val (v, bsp)= decode(r2, bs1::Nil)
            (RIGHT(v), bsp)
        } 

      case _ => (EMPTY, bs) // maybe not needed
*/
  
/*</script>*/ /*<generated>*/
def $main() = { _root_.scala.Iterator[String]() }
  override def toString = "marked$minusBitcodeTest"
  /*</generated>*/
}
