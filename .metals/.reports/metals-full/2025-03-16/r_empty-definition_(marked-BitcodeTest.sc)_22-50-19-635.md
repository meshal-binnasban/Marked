error id: file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-BitcodeTest.sc:335
file://<HOME>/Google%20Drive/KCL/Code%20Playground/Marked/marked-BitcodeTest.sc
empty definition using pc, found symbol in pc: 
semanticdb not found
|empty definition using fallback
non-local guesses:
	 -Rexp.SEQ.
	 -Rexp.SEQ#
	 -Rexp.SEQ().
	 -VALUE.SEQ.
	 -VALUE.SEQ#
	 -VALUE.SEQ().
	 -SEQ.
	 -SEQ#
	 -SEQ().
	 -scala/Predef.SEQ.
	 -scala/Predef.SEQ#
	 -scala/Predef.SEQ().

Document text:

```scala
import scala.compiletime.ops.string

enum Rexp {
  case ZERO
  case ONE(bs: List[Int] = List())
  case CHAR(c: Char ,marked:Boolean =false ,bs: List[Int] = List()) //marked: Int = 0
  case ALT(r1: Rexp, r2: Rexp , bs: List[Int] = List())
  case SEQ(r1: Rexp, r2: Rexp , bs: List[Int] = List())
  case STAR(r: Rexp ,   bs: List[Int] = List())
  case NTIMES(r: Rexp, n: Int,nmark:List[Int] = List(0) , counter:Int=0 , bs: List[Int] = List()) 
  case INIT(r: Rexp , bs: List[Int] = List())
  
  override def toString: String = this match {
    case ZERO => "ZERO"
    case ONE(bs) => s"ONE bs:$bs"
    case CHAR(c, marked ,bs) => s"CHAR($c , $marked $bs)"
    case ALT(r1, r2,bs) => s"ALT($r1, $r2) bs:$bs"
    case SEQ(r1, r2,bs) => s"SEQ($r1, $r2) bs:$bs"
    case STAR(r,bs) => s"STAR($r) bs:$bs"
    case NTIMES(r, n, nmark ,counter,bs) => s"NTIMES($r, n=$n, nmark=$nmark , count=$counter , bs:$bs)"
    case INIT(r,bs) => s"INIT($r) bs:$bs"
  }
}
import Rexp._

def OPT(r: Rexp, bs:List[Int]): Rexp = ALT(r, ONE(bs), bs)
def intern2(r: Rexp,bs:List[Int]) : Rexp = INIT(r,bs)

def shift(mark: Boolean,re: Rexp, c: Char ): Rexp = re match {
    case ZERO => ZERO
    case ONE(bs)=> ONE(bs)
    case CHAR(ch,marked,bs) => {
        CHAR(ch,mark && ch == c, bs) 
    }
    case ALT(r1, r2,bs) => ALT(shift(mark,r1,c),shift(mark,r2,c)) //fuse left ?
    case SEQ(r1,r2,bs) =>
      if (mark && nullable(r1))
      {
      //println("if case")
        ALT(SEQ(shift(mark, r1, c), shift(fin(r1), r2, c)), shift(1, r2, c))
      }
      else{
      //println("else case")
        SEQ(shift(mark, r1, c), shift(fin(r1), r2, c))   
      }  
    //  SEQ (shift(mark,r1,c), shift(mark * nullable(r1) + fin(r1),r2,c))
    case STAR(r,bs) => STAR(shift(mark || fin(r), r,c))
    case NTIMES(r, n,nmark,counter,bs) => // maybe shift false if n==0 or counter == n?
      if (n==0){
        ONE(bs)
      }else{
        if(counter == n){
           val test=shift(mark||fin(r), r, c)
           val v= if(fin(test)) 1 else 0
           NTIMES(test, n,v ::nmark,counter) // maybe we need to shift?
        } else 
            if (mark || fin(r))
            { 
              
              NTIMES(shift(mark || fin(r), r, c), n , 1::nmark,counter+1)
            }
            else NTIMES(shift(false, r, c), n,0::nmark,counter)
      }  
    case INIT(r,bs) => shift(true, r, c)
     
}

def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE(bs) => true
  case CHAR(_,_,_) => false
  case ALT(r1, r2,bs) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2,bs) => nullable(r1) && nullable(r2)
  case STAR(_,_) => true
  case NTIMES(r, n ,nmark , counter ,bs) => if (n == 0) true else nullable(r) //?
  case INIT(r,bs) => nullable(r)
}

def fin(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE(bs) => false
  case CHAR(c,marked,bs) => marked
  case ALT(r1, r2,bs) => fin(r1) || fin(r2)
  case SEQ(r1, r2,bs) => (fin(r1) && nullable(r2)) || fin(r2) 
  case STAR(r,bs) => fin(r)
  case NTIMES(r,n,nmark , counter,bs) => // we check the first n marks in nmarks, and if they each more than one and not more than n
    if(n == counter || (counter<n && nullable(r))) 
    fin(r) else false
   // if(nmark >= n) fin(r) else 0 // 
}

def mat(r: Rexp, s: List[Char]) : Rexp = s match {
  case Nil => r
  case c::cs => mat(shift(false, r, c), cs)
}

def matcher(r: Rexp, s: List[Char]) : Boolean =
  if (s == Nil) nullable(r) else fin(mat(r, s))


def matcher2(r: Rexp, s: List[Char]) : Rexp =
  val x=mat(r, s)
  x match {
    case INIT(r,bs) => 
      //println(s"Final Reg=$r")
      r
    case _ => 
     // println(s"Final Reg=$x")
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

def mkeps_marked(r: Rexp): List[Int] = r match {
    case ONE (bs) => bs // ???
    case CHAR(c,marked,bs) => if(marked) bs else List()
    case ALT(r1, r2,bs) =>
        //bs @ (mkeps r1) if nullable(r1)
        if (nullable(r1))  (bs ++ mkeps_marked(r1))
        //bs @ (mkeps r2)
        else (bs ++ mkeps_marked(r2))
    case SEQ(r1, r2,bs) => (bs ++ mkeps_marked(r1) ++ mkeps_marked(r2))
    case STAR(r1,bs) => (bs ++ List(1))
    case NTIMES(r, n, nmark,counter,bs) =>
      if (n == 0) mkeps_marked(ONE(bs))
      else if (counter == n) (bs ++ List(1)) //???
      else bs // ????? 
  }


@main
def test00() = {
  
  val a=CHAR('a')
  val b=CHAR('b')
  val rexp=NTIMES(   ALT(a ,ALT(a ,b)) , 2  )

  val ss="ab".toList
 // val result=matcher(intern2(rexp),ss)
 // println("Testing mkeps \n")
 // println(s" rexp=$rexp Result=$result\n")

  val finReg=matcher2(intern2(rexp,List()),ss)
  println(s"finReg=$finReg\n")
  println(mkeps_marked(finReg))
  println("\n========================\n")

}

//testing NTIMES and SEQ NTIMES.
@main
def test01() = {
    val n=2
    val rexp=intern2(EVIL1(n), List())

    val ss="aaaaa".toList
    println(s"input is $ss \n")

    println(s"NTIMES :\n")
    println(s"\n Original NTIMES\n ${pp(rexp)}")

    val result=matcher(rexp,ss)
    val finReg=matcher2(rexp,ss)

    println(s"\n Final NTIMES\n ${pp(finReg)}")
    println(s"\n NTIMES Result=$result and Size=${size(finReg)} \n \n")

   // val regsList=extractStages(finReg,ss)

    //for ((ch,regex) <- regsList) {
    //  println(s"\nInput Character: $ch")
    //  println(s"Regex: $regex \n")
   // }

    println("\n========================\n")

  
    val testRexp=intern2(SeqNTIMES(EVIL1(n)),List())
    println(s"input is $ss \n")

    println(s"Original SEQ :\n")
    println(s"\n ${pp(testRexp)}")

    val resultSeq=matcher(testRexp,ss)
    val finReg2=matcher2(testRexp,ss)

    println(s"\n Final SEQ: \n ${pp(finReg2)}")
    println(s"\n SEQ rexp :Result=$resultSeq Size=${size(finReg2)} \n\n")
/*
    val regsList=extractStages(finReg2,ss)

    for ((ch,regex) <- regsList) {
      println(s"\nInput Character: $ch")
      println(s"Regex: $regex \n")
      println(pp(regex))
    }
*/

}

@main
def test02() = {
  val a=CHAR('a')
  val b=CHAR('b')
  val one=ONE()
  val rexp=NTIMES(   ALT( a , one ) , 2  )

  val ss="aab".toList
  val result=matcher(intern2(rexp,List()),ss)
  println(s" rexp=$rexp Result=$result\n")

  val finReg=matcher2(intern2(rexp,List()),ss)
  println(s"finReg=${pp(finReg)}\n")

}

@main
def test03() = {
  val a=CHAR('a')
  val b=CHAR('b')
  val rexp=SEQ(a , b)

  val shiftedOnce=shift(true,rexp,'a')
  val shiftedTwice=shift(true,shiftedOnce,'b')
  println(s"Shifted Once: ${pp(shiftedTwice)}\n")

  val unshiftedOnce=unshift(true,shiftedTwice,'a')
  println(s"Unshifted Once: ${pp(unshiftedOnce)}\n")

  println(s"unshift using basic unshift2 : ${pp(unshift2(shiftedTwice))}\n")

  println("\n========================\n")
  val rexp2=STAR(a)
  val shiftedOnce2=shift(true,rexp2,'a')
  println(s"Shifted Once: ${pp(shiftedOnce2)}\n")

  val unshiftedOnce2=unshift(1,shiftedOnce2,'a')
  println(s"Unshifted Once: ${pp(unshiftedOnce2)}\n")

}

def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE(bs) => 1
  case CHAR(_,_,_) => 1
  case ALT(r1, r2,bs) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2,bs) => 1 + size(r1) + size(r2)
  case STAR(r,bs) => 1 + size(r)
  case NTIMES(r,n, nmark,counter,bs) => 1 + size(r) 
  case INIT(r,bs) => 1 + size(r)   
}

def SeqNTIMES(r: Rexp): Rexp = r match {
  case ZERO => ZERO
  case ONE(bs)  => ONE(bs)
  case CHAR(c, marked,bs) => CHAR(c, marked,bs)
  case ALT(r1, r2,bs) =>
    ALT(SeqNTIMES(r1), SeqNTIMES(r2),bs)
  case SEQ(r1, r2,bs) =>
    SEQ(SeqNTIMES(r1), SeqNTIMES(r2),bs)
  case STAR(r1,bs) =>
    STAR(SeqNTIMES(r1),bs)
  case NTIMES(r, n, nmark,counter,bs) =>
    val expandR = SeqNTIMES(r)
    if (n <= 0) ONE(bs) else
        if (n == 1) expandR else 
            { SeqNTIMES( SEQ( expandR, NTIMES(expandR,n-1,nmark,counter,bs) ,bs) ) }
  case INIT(r,bs) => ZERO // so match doesn't give a warning
}

def EVIL1(n: Int) = SEQ(NTIMES(OPT(CHAR('a'),List()), n), NTIMES(CHAR('a'), n))
val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

//@arg(doc = "Test (a?{n}) (a{n})")

def test1() = {
  for (i <- 0 to 11000 by 1000) {
    println(f"$i: ${time_needed(2, matcher(EVIL1(i), ("a" * i).toList))}%.5f")
  }
}

//@arg(doc = "Test (a*)* b")

def test2() = {
  for (i <- 0 to 7000000 by 500000) {
    println(f"$i: ${time_needed(2, matcher(EVIL2, ("a" * i).toList))}%.5f")
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

def pp(e: Rexp) : String = e match {
  case ZERO => "0\n"
  case ONE(bs) => "1 bs: $bs\n"
  case CHAR(c,marked) => if (marked.head>=1) s"•$c m:$marked\n" else s"$c m:$marked\n"
  case ALT(r1, r2) => "ALT\n" ++ pps(r1, r2)
  case SEQ(r1, r2) => "SEQ\n" ++ pps(r1, r2)
  case STAR(r) => "STAR\n" ++ pps(r) //if(n == 0 ) fin(r) 
  case NTIMES(r,n,nmark,counter) => if (n == 0 && fin(r) == 1) s"• NTIMES(n=$n) $nmark counter=$counter\n" ++ pps(r) 
    else s"NTIMES(n=$n) $nmark counter=$counter\n" ++ pps(r)
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

// maybe we dont'need mark in unshift
def unshift(mark: Int, r: Rexp, c: Char): Rexp = r match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(ch,marked) => 
    //  m && (d == c) from shift
    //revers ? (m || d != c, d)
    val newMark = if (ch == c){
                    if(mark>=1)
                     0 else
                       1 * marked.head
                       }
                       else
                        0
    CHAR(ch, newMark :: marked.tail) 
  case ALT(r1, r2) => ALT(unshift(mark, r1, c), unshift(mark, r2, c))
  case SEQ(r1, r2) =>
      SEQ(unshift(mark, r1, c), unshift(fin(r1), r2, c))
  case STAR(r) =>
    STAR(unshift(mark, r, c)) // STAR(unshift(mark * fin(r), r, c))
  
  case NTIMES(r, n,nmark,counter) =>
    if (n == 0) NTIMES(r, n,nmark,counter) else {
      if (mark < 1 || fin(r) < 1) {
        val m = mark * fin(r)
        NTIMES(unshift(m, r, c), n, m::nmark,counter-1)
      } else {
        NTIMES(unshift(0, r, c), n, 0::nmark,counter)
      }
    }
}

def unshift2 (r:Rexp): Rexp = r match {
  case ZERO => ZERO
  case ONE => ONE
  case CHAR(ch,marked) => CHAR(ch,marked.tail)
  case ALT(r1, r2) => ALT(unshift2(r1), unshift2(r2))
  case SEQ(r1, r2) => SEQ(unshift2(r1), unshift2(r2))
  case STAR(r) => STAR(unshift2(r))
  case NTIMES(r, n,nmark,counter) => NTIMES(unshift2(r), n,nmark,counter)

}



```

#### Short summary: 

empty definition using pc, found symbol in pc: 