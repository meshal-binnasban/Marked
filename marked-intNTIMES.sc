import scala.compiletime.ops.string

enum Rexp {
  case ZERO
  case ONE 
  case CHAR(c: Char , marked: List[Int] = List(0)) //marked: Int = 0
  case ALT(r1: Rexp, r2: Rexp )
  case SEQ(r1: Rexp, r2: Rexp)
  case STAR(r: Rexp)
  case NTIMES(r: Rexp, n: Int,nmark:List[Int] = List(0)) 
  case INIT(r: Rexp)
  
  override def toString: String = this match {
    case ZERO => "ZERO"
    case ONE => "ONE"
    case CHAR(c, marked) => s"CHAR($c , $marked)"
    case ALT(r1, r2) => s"ALT($r1, $r2)"
    case SEQ(r1, r2) => s"SEQ($r1, $r2)"
    case STAR(r) => s"STAR($r)"
    case NTIMES(r, n, nmark) => s"NTIMES($r, n=$n, nmark=$nmark)"
    case INIT(r: Rexp) => s"INIT($r)"
  }
}
import Rexp._

def OPT(r: Rexp) = ALT(r, ONE)

def shift(mark: Int,re: Rexp,c: Char ): Rexp = re match {
    case ZERO => ZERO
    case ONE=> ONE
    case CHAR(ch,marked) => {
     // CHAR(ch, if(ch==c) 1 * mark else 0 * mark )
    val newMark = if (ch == c) 1 * mark else 0 * mark
    CHAR(ch, newMark :: marked) 
    }
    case ALT(r1, r2) => ALT(shift(mark,r1,c),shift(mark,r2,c))
    case SEQ(r1,r2) => SEQ (shift(mark,r1,c), shift(mark * nullable(r1) + fin(r1),r2,c))
    case STAR(r) => STAR(shift(mark + fin(r), r,c))
    case NTIMES(r, n,nmark) =>
     if (n == 0) ONE  
      else {
       val shiftedR=shift(mark+fin(r),r,c)
     //  if (fin(shiftedR) >= 1) 
        NTIMES(shiftedR, n, 1 ::nmark)
      // else
      //  NTIMES(shiftedR, n, nmark)     
      }   
/*      
      if (n == 0) ONE  // already matched n times
        else {
          if(nullable(r) == 1)
            {val shiftedNullableR = shift(mark,r,c)
            if (fin(shiftedNullableR) >= 1) 
            NTIMES(shiftedNullableR, n, nmark + 1)
            else NTIMES(shiftedNullableR, n, nmark)
            }
            else
              {
                val shiftedR=shift(mark*fin(r),r,c)
                if (fin(shiftedR) >= 1) 
                  NTIMES(shiftedR, n, nmark + 1)
                else NTIMES(shiftedR, n, nmark)
            }       
      }
*/
    case INIT(r) => shift(1, r, c)
     
}

def nullable(r: Rexp) : Int = r match {
  case ZERO => 0
  case ONE => 1
  case CHAR(_,_) => 0
  case ALT(r1, r2) => nullable(r1) + nullable(r2)
  case SEQ(r1, r2) => nullable(r1) + nullable(r2)
  case STAR(_) => 1
  case NTIMES(r, n ,nmark ) => if (n == 0) 1 else nullable(r) //?
  case INIT(r) => nullable(r)
}

def fin(r: Rexp) : Int = r match {
  case ZERO => 0
  case ONE => 0
  case CHAR(c,marked) => marked.head
  case ALT(r1, r2) => fin(r1) + fin(r2)
  case SEQ(r1, r2) => (fin(r1) * nullable(r2)) + fin(r2)
  case STAR(r) => fin(r)
  case NTIMES(r,n,nmark) => 
    if(n == nmark.length -1) 1 else 0 
   // if(nmark >= n) fin(r) else 0 // 
}


def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => mat(shift(0, r, c), cs)
}

def matcher(r: Rexp, s: List[Char]) : Int =
  if (s == Nil) nullable(r) else 
    val x=mat(r, s)
    println(s"Final Reg=$x")
    fin(x)

def matcher2(r: Rexp, s: List[Char]) : Rexp =
  val x=mat(r, s)
  x match {
    case INIT(r) => 
      println(s"Final Reg=$r")
      r
    case _ => 
      println(s"Final Reg=$x")
      x
  }
  



enum VALUE {
  case ZEROV
  case EMPTY
  case CHARV(c: Char)
  case UNMARKED(s:String)
  case SEQV(v1: VALUE, r2: VALUE )
  case LEFT(v: VALUE)
  case RIGHT(v: VALUE)
  case STARV(vs: List[VALUE])
  

}
import VALUE._


def mkeps_marked(r: Rexp): VALUE = r match {
    case ONE => EMPTY
    case CHAR(c,marked) => if(marked.head >=1) CHARV(c)
      else UNMARKED(c.toString())
    case ALT(r1, r2) =>
      if (fin(r1) == 1) LEFT(mkeps_marked(r1))
      else if (fin(r2) == 1) RIGHT(mkeps_marked(r2))
      else UNMARKED("ALT")
    case SEQ(r1, r2) => SEQV(mkeps_marked(r1), mkeps_marked(r2))
    case STAR(r1) =>
      if (fin(r1) == 1) STARV(List(mkeps_marked(r1)))
      else STARV(Nil)
    case ZERO => ZEROV
  }








def intern2(r: Rexp) : Rexp = INIT(r)


@main
def test00() = {
  
  //SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))
  val a=CHAR('a')
  val b=CHAR('b')
  //val rexp=ALT( SEQ(a, ALT(ONE, b)) , SEQ(a , b))
  val rexp=STAR(   ALT(a ,SEQ(a ,b))   )

  val ss="aab".toList
  val result=matcher(intern2(rexp),ss)
  println("Testing mkeps \n")
  println(s" rexp=$rexp Result=$result\n")

  val finReg=matcher2(intern2(rexp),ss)
  println(mkeps_marked(finReg))

}
@main
def test01() = {
    val n=2
    val rexp=EVIL1(n)

    val ss="aaa".toList

    println(s"NTIMES : rexp=$rexp")
    val result=matcher(intern2(rexp),ss)
    println(s" NTIMES Result=$result\n")
    println("========================")

    val rexpSeq=intern2(SeqNTIMES(rexp))
    println(s"SEQ : rexpSeq=$rexpSeq")
    val resultSeq=matcher(rexpSeq,ss)
    println(s"\n Result=$resultSeq")
}
@main
def test02() = {
    val n=2
    val rexp=EVIL1(n)
    println("=====Test====")
    val br1 = intern2(rexp)
    val s = "aaa".toList
    println(s"s is $s \n rexp= $br1 \n ")

    for (i <- s.indices) {
    println(s"${i + 1}- =shift ${s(i)}=")
    val sPart = s.take(i + 1)
    println(pp(mat(br1, sPart)))
    }
    println(matcher(br1,s))
}

def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_,_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r,n, nmark) => 1 + size(r) 
  case INIT(r) => 1 + size(r)   
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
  case NTIMES(r, n, _) =>
    val expandR = SeqNTIMES(r)
    if (n <= 0) ONE else
        if (n == 1) expandR else 
            { SeqNTIMES( SEQ( expandR, NTIMES(expandR,n-1) ) ) }
  case INIT(r) => ZERO
}

def EVIL1(n: Int) = SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))
val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

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
    println(f"$i: ${time_needed(2, matcher(EVIL1(i), ("a" * i).toList))}%.5f")
  }
}

//@arg(doc = "Test (a*)* b")
@main
def test2() = {
  for (i <- 0 to 7000000 by 500000) {
    println(f"$i: ${time_needed(2, matcher(EVIL2, ("a" * i).toList))}%.5f")
  }
} 

//@arg(doc = "All tests.")
@main
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

def pp(e: Rexp) : String = e match {
  case ZERO => "0\n"
  case ONE => "1\n"
  case CHAR(c,marked) => if (marked.head>=1) s"•$c m:$marked\n" else s"$c\n"
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r)
  case NTIMES(r,n,nmark) => if (nmark.length == n) s"• NTIMES(n=$n) $nmark\n" ++ pps(r) 
    else s"NTIMES(n=$n) $nmark\n" ++ pps(r)
  case INIT(r) => "INIT\n" ++ pps(r)
}
def pps(es: Rexp*) = indent(es.map(pp))


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